// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::{collections::HashSet, fmt::Display, str::FromStr};

use crate::{
    core::{DevId, Device, DeviceInfo, DmName, DmOptions, DmUuid, DM},
    result::{DmError, DmResult, ErrorEnum},
    shared::{
        device_create, device_exists, device_match, parse_device, DmDevice, TargetLine,
        TargetParams, TargetTable, TargetTypeBuf,
    },
    units::Sectors,
};

const SNAPSHOT_TARGET_NAME: &str = "snapshot";

/// Snapshot target persistent flag value variants
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum SnapshotPersistent {
    /// P (Persistent - will survice after reboot)
    Persistent,
    /// N (Not persistent - will not survive after reboot)
    NonPersistent,
    /// O (Overflow) can be added as a persistent store option to allow userspace to advertise its
    /// support for seeing “Overflow” in the snapshot status.
    PersistentWithOverflowSupport,
}

impl Display for SnapshotPersistent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SnapshotPersistent::Persistent => f.write_str("P"),
            SnapshotPersistent::NonPersistent => f.write_str("N"),
            SnapshotPersistent::PersistentWithOverflowSupport => f.write_str("PO"),
        }
    }
}

//TODO: implement support for features
//Optional features:
// discard_zeroes_cow - a discard issued to the snapshot device that maps to entire chunks to will
// zero the corresponding exception(s) in the snapshot’s exception store.
// discard_passdown_origin - a discard to the snapshot device is passed down to the snapshot-origin’s underlying device. This
// doesn’t cause copy-out to the snapshot exception store because the snapshot-origin target is
// bypassed.
// ref: https://www.kernel.org/doc/html/latest/admin-guide/device-mapper/snapshot.html

/// Struct representing params for snapshot target
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SnapshotTargetParams {
    origin_device: Device,
    cow_device: Device,
    persistent: SnapshotPersistent,
    chunksize: usize,
    feature_args: HashSet<FeatureArg>,
}

impl TargetParams for SnapshotTargetParams {
    fn param_str(&self) -> String {
        let feature_string = if self.feature_args.is_empty() {
            "".to_owned()
        } else {
            format!(
                " {}",
                self.feature_args
                    .iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
                    .join(" ")
            )
        };
        format!(
            "{} {} {} {}{}",
            self.origin_device, self.cow_device, self.persistent, self.chunksize, feature_string
        )
    }

    fn target_type(&self) -> TargetTypeBuf {
        TargetTypeBuf::new(SNAPSHOT_TARGET_NAME.into()).expect("SNAPSHOT_TARGET_NAME is valid")
    }
}

impl SnapshotTargetParams {
    /// Try to create new snapshot target param
    ///
    /// * `origin_device` - block device which will be snapshoted
    /// * `cow_device` - block device which will store changed chunks of sectors
    /// * `persistent` - whether the device created will survive reboot or not
    /// * `chunksize` - changed chunks of `chunksize` sectors will be stored on the `cow_device`
    pub fn try_new(
        origin_device: &str,
        cow_device: &str,
        persistent: SnapshotPersistent,
        chunksize: usize,
        feature_args: HashSet<FeatureArg>,
    ) -> DmResult<Self> {
        Ok(Self {
            origin_device: parse_device(origin_device, "Unable to parse origin device")?,
            cow_device: parse_device(cow_device, "Unable to parse COW device")?,
            persistent,
            chunksize,
            feature_args,
        })
    }
}

impl Display for SnapshotTargetParams {
    /// Generate params to be passed to DM.  The format of the params is:
    ///
    /// ```plain
    ///
    ///     snapshot <origin> <COW device> <persistent?> <chunksize> [<# feature args> [<arg>]*]
    ///
    /// A snapshot of the <origin> block device is created. Changed chunks of <chunksize> sectors
    /// will be stored on the <COW device>. Writes will only go to the <COW device>. Reads will
    /// come from the <COW device> or from <origin> for unchanged data. <COW device> will often be
    /// smaller than the origin and if it fills up the snapshot will become useless and be
    /// disabled, returning errors. So it is important to monitor the amount of free space and
    /// expand the <COW device> before it fills up.
    ///
    /// <persistent?> is P (Persistent) or N (Not persistent - will not survive after reboot). O
    /// (Overflow) can be added as a persistent store option to allow userspace to advertise its
    /// support for seeing “Overflow” in the snapshot status. So supported store types are “P”,
    /// “PO” and “N”.
    ///
    /// The difference between persistent and transient is with transient snapshots less metadata
    /// must be saved on disk - they can be kept in memory by the kernel.
    ///
    /// When loading or unloading the snapshot target, the corresponding snapshot-origin or
    /// snapshot-merge target must be suspended. A failure to suspend the origin target could
    /// result in data corruption.
    ///
    /// Optional features:
    ///
    ///     discard_zeroes_cow - a discard issued to the snapshot device that maps to entire chunks
    ///     to will zero the corresponding exception(s) in the snapshot’s exception store.
    ///
    ///     discard_passdown_origin - a discard to the snapshot device is passed down to the
    ///     snapshot-origin’s underlying device. This doesn’t cause copy-out to the snapshot
    ///     exception store because the snapshot-origin target is bypassed.
    ///
    ///     The discard_passdown_origin feature depends on the discard_zeroes_cow feature being
    ///     enabled.

    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", SNAPSHOT_TARGET_NAME, self.param_str())
    }
}

/// Snapshot target optional feature parameters
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum FeatureArg {
    /// A discard issued to the snapshot device that maps to entire chunks to will zero the corresponding exception(s) in the snapshot’s exception store.
    DiscardZeroesCow,
    /// A discard to the snapshot device is passed down to the snapshot-origin’s underlying device. This doesn’t cause copy-out to the snapshot exception store because the snapshot-origin target is bypassed.
    /// The discard_passdown_origin feature depends on the discard_zeroes_cow feature being enabled.
    DiscardPassdownOrigin,
}

impl FeatureArg {
    /// Returns empty set of snapshot feature args
    pub fn default() -> HashSet<Self> {
        HashSet::new()
    }
}

impl Display for FeatureArg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            FeatureArg::DiscardZeroesCow => write!(f, "discard_zeroes_cow"),
            FeatureArg::DiscardPassdownOrigin => write!(f, "discard_passdown_origin"),
        }
    }
}

impl FromStr for SnapshotTargetParams {
    type Err = DmError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        fn parse_feature_args(vals: &[&str]) -> DmResult<HashSet<FeatureArg>> {
            let mut res = HashSet::new();
            for &val in vals {
                match val {
                    "discard_zeroes_cow" => res.insert(FeatureArg::DiscardZeroesCow),
                    "discard_passdown_origin" => res.insert(FeatureArg::DiscardPassdownOrigin),
                    x => return Err(DmError::Dm(ErrorEnum::Invalid, format!("Invalid snapshot feature arg value: {}, supported values: 'discard_zeroes_cow', 'discard_passdown_origin'",x))),
                };
            }
            if res.contains(&FeatureArg::DiscardPassdownOrigin)
                && !res.contains(&FeatureArg::DiscardZeroesCow)
            {
                return Err(DmError::Dm(
                    ErrorEnum::Invalid,
                    "discard_zeroes_cow needs to be set in order to use discard_passdown_origin"
                        .into(),
                ));
            }
            Ok(res)
        }
        let params = s.split(' ').collect::<Vec<_>>();
        let params_len = params.len();
        if !(5..=7).contains(&params_len) {
            return Err(DmError::Dm(
                ErrorEnum::Invalid,
                format!(
                    "Invalid number of parameters: {}, expecting between 5 to 7 parameters",
                    params_len
                ),
            ));
        }
        let snapshot_string = params[0];
        if snapshot_string != "snapshot" {
            return Err(DmError::Dm(
                ErrorEnum::Invalid,
                format!("expecting 'snapshot' string, got '{}'", snapshot_string),
            ));
        }
        let device_string = params[1];
        let origin = parse_device(device_string, "block device for snapshot origin")?;

        let cow_string = params[2];
        let cow_device = parse_device(cow_string, "block device for snapshot COW device")?;
        let persitent_string = params[3];

        let persistent: SnapshotPersistent = match persitent_string {
            "P" => Ok(SnapshotPersistent::Persistent),
            "N" => Ok(SnapshotPersistent::NonPersistent),
            "PO" => Ok(SnapshotPersistent::PersistentWithOverflowSupport),
            _ => Err(DmError::Dm(
                ErrorEnum::Invalid,
                format!(
                    "Invalid persistent flag value: {}, expected one of: 'P', 'N', 'PO'",
                    persitent_string
                ),
            )),
        }?;

        let chunksize_string = params[4];
        let chunksize = usize::from_str(chunksize_string).map_err(|err| {
            DmError::Dm(
                ErrorEnum::Invalid,
                format!("Invalid <chunksize> value: {}, {}", chunksize_string, err),
            )
        })?;

        let feature_args = if params.len() > 5 {
            parse_feature_args(&params[5..])?
        } else {
            HashSet::new()
        };

        Ok(SnapshotTargetParams {
            origin_device: origin,
            cow_device,
            persistent,
            chunksize,
            feature_args: feature_args.into_iter().collect::<HashSet<_>>(),
        })
    }
}

/// A target table for a snapshot device.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SnapshotDevTargetTable {
    /// The snapshot device table
    pub table: Vec<TargetLine<SnapshotTargetParams>>,
}

impl SnapshotDevTargetTable {
    fn new(start: Sectors, length: Sectors, params: SnapshotTargetParams) -> Self {
        Self {
            table: vec![TargetLine::new(start, length, params)],
        }
    }
}

impl Display for SnapshotDevTargetTable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for line in &self.table {
            writeln!(f, "{} {} {}", line.start, line.length, line.params)?;
        }
        Ok(())
    }
}

impl TargetTable for SnapshotDevTargetTable {
    fn from_raw_table(table: &[(u64, u64, String, String)]) -> DmResult<Self> {
        if table.len() != 1 {
            let err_msg = format!(
                "SnapshotDev table should have exactly one line, has {} lines",
                table.len()
            );
            return Err(DmError::Dm(ErrorEnum::Invalid, err_msg));
        }

        let line = table.first().expect("table.len() == 1");
        Ok(SnapshotDevTargetTable::new(
            Sectors(line.0),
            Sectors(line.1),
            format!("{} {}", line.2, line.3).parse::<SnapshotTargetParams>()?,
        ))
    }

    fn to_raw_table(&self) -> Vec<(u64, u64, String, String)> {
        self.table
            .iter()
            .map(|x| {
                (
                    *x.start,
                    *x.length,
                    x.params.target_type().to_string(),
                    x.params.param_str(),
                )
            })
            .collect::<Vec<_>>()
    }
}

/// An abstraction for device mapper snapshot device
#[derive(Debug)]
pub struct SnapshotDev {
    dev_info: Box<DeviceInfo>,
    table: SnapshotDevTargetTable,
}

impl SnapshotDev {
    /// Constructs a new block device in which <COW device> will be used for writing changed
    /// chunks of sectors. Writes will only go to <COW device>. Read will come from <COW device>
    /// for changed data and from <origin> for unchanged data. If <COW device> fills up, the
    /// snapshot will become useless and disabled, returning errors. So it is important to monitor the amount of free space and expand the <COW device> before it fills up.
    pub fn setup(
        dm: &DM,
        name: &DmName,
        uuid: Option<&DmUuid>,
        start: Sectors,
        length: Sectors,
        params: SnapshotTargetParams,
        options: DmOptions,
    ) -> DmResult<SnapshotDev> {
        let table = SnapshotDevTargetTable::new(start, length, params);
        let dev = if device_exists(dm, name)? {
            let dev_info = dm.device_info(&DevId::Name(name))?;
            println!(
                "Loaded flags: {:#?}, passed flags: {:#?}",
                dev_info.flags(),
                options.flags()
            );
            let dev = SnapshotDev {
                dev_info: Box::new(dev_info),
                table,
            };
            device_match::<SnapshotDevTargetTable, SnapshotDev>(dm, &dev, uuid)?;
            dev
        } else {
            let dev_info = device_create(dm, name, uuid, &table, options)?;
            SnapshotDev {
                dev_info: Box::new(dev_info),
                table,
            }
        };
        Ok(dev)
    }
}

impl DmDevice<SnapshotDevTargetTable> for SnapshotDev {
    fn device(&self) -> Device {
        self.dev_info.device()
    }

    fn devnode(&self) -> std::path::PathBuf {
        ["/dev", &format!("dm-{}", self.dev_info.device().minor)]
            .iter()
            .collect()
    }

    fn equivalent_tables(
        left: &SnapshotDevTargetTable,
        right: &SnapshotDevTargetTable,
    ) -> DmResult<bool> {
        Ok(left == right)
    }

    fn name(&self) -> &DmName {
        match self.dev_info.name() {
            Some(name) => name,
            None => panic!("Name is required for device"),
        }
    }

    fn size(&self) -> Sectors {
        self.table.table.iter().map(|l| l.length).sum()
    }

    fn table(&self) -> &SnapshotDevTargetTable {
        &self.table
    }

    fn teardown(&mut self, dm: &DM) -> DmResult<()> {
        dm.device_remove(&DevId::Name(self.name()), DmOptions::default())?;
        Ok(())
    }

    fn uuid(&self) -> Option<&DmUuid> {
        self.dev_info.uuid()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::Path;

    use crate::testing::{test_name, test_with_spec};

    /// Verify that you can create snapshot device
    fn test_setup(paths: &[&Path]) {
        let dm = DM::new().unwrap();
        let name = test_name("snapshot").expect("valid format");
        let params = SnapshotTargetParams::try_new(
            paths[0].to_str().unwrap(),
            paths[1].to_str().unwrap(),
            SnapshotPersistent::Persistent,
            8,
            FeatureArg::default(),
        )
        .unwrap();

        let mut snapshot_dev = SnapshotDev::setup(
            &dm,
            &name,
            None,
            Sectors(0),
            Sectors(1),
            params,
            DmOptions::default(),
        )
        .unwrap();
        snapshot_dev.teardown(&dm).unwrap();
    }

    /// Verify that when you create a device with the same name the second time and you use the
    /// same origin_device, cow_device and params then the setup will succeed
    fn test_setup_same_device_twice(paths: &[&Path]) {
        let dm = DM::new().unwrap();
        let name = test_name("snapshot").expect("valid format");
        let params = SnapshotTargetParams::try_new(
            paths[0].to_str().unwrap(),
            paths[1].to_str().unwrap(),
            SnapshotPersistent::Persistent,
            8,
            FeatureArg::default(),
        )
        .unwrap();

        SnapshotDev::setup(
            &dm,
            &name,
            None,
            Sectors(0),
            Sectors(1),
            params.clone(),
            DmOptions::default(),
        )
        .unwrap();

        let mut snapshot_dev = SnapshotDev::setup(
            &dm,
            &name,
            None,
            Sectors(0),
            Sectors(1),
            params,
            DmOptions::default(),
        )
        .unwrap();
        snapshot_dev.teardown(&dm).unwrap();
    }

    /// Verify that when you create a device with the same name the second time and
    /// the params differ, the setup will fail
    fn test_setup_twice_same_name_different_target_params(paths: &[&Path]) {
        let dm = DM::new().unwrap();
        let name = test_name("snapshot").expect("valid format");
        let params1 = SnapshotTargetParams::try_new(
            paths[0].to_str().unwrap(),
            paths[1].to_str().unwrap(),
            SnapshotPersistent::Persistent,
            8,
            FeatureArg::default(),
        )
        .unwrap();
        let params2 = SnapshotTargetParams::try_new(
            paths[0].to_str().unwrap(),
            paths[1].to_str().unwrap(),
            SnapshotPersistent::NonPersistent,
            8,
            FeatureArg::default(),
        )
        .unwrap();

        SnapshotDev::setup(
            &dm,
            &name,
            None,
            Sectors(0),
            Sectors(1),
            params1,
            DmOptions::default(),
        )
        .unwrap();
        SnapshotDev::setup(
            &dm,
            &name,
            None,
            Sectors(0),
            Sectors(1),
            params2,
            DmOptions::default(),
        )
        .unwrap();
    }

    #[test]
    fn loop_test_setup() {
        test_with_spec(2, test_setup);
    }
    #[test]
    fn loop_test_setup_same_device_twice() {
        test_with_spec(2, test_setup_same_device_twice);
    }
    #[test]
    #[should_panic]
    fn loop_test_setup_twice_same_name_different_target_params() {
        test_with_spec(2, test_setup_twice_same_name_different_target_params);
    }

    #[test]
    fn test_snapshot_target_params_no_feature_args() {
        "snapshot 255:1 255:1 P 8"
            .parse::<SnapshotTargetParams>()
            .unwrap();
    }

    #[test]
    fn test_snapshot_target_params_discard_zeroes_cow() {
        "snapshot 255:1 255:1 P 8 discard_zeroes_cow"
            .parse::<SnapshotTargetParams>()
            .unwrap();
    }

    #[test]
    fn test_snapshot_target_params_discard_zeroes_cow_and_discard_passdown_origin() {
        "snapshot 255:1 255:1 P 8 discard_zeroes_cow discard_passdown_origin"
            .parse::<SnapshotTargetParams>()
            .unwrap();
    }

    #[test]
    #[should_panic]
    fn test_snapshot_target_params_discard_passdown_origin_but_no_discard_zeroes_cow() {
        "snapshot 255:1 255:1 P 8 discard_passdown_origin"
            .parse::<SnapshotTargetParams>()
            .unwrap();
    }

    #[test]
    #[should_panic]
    fn test_snapshot_target_params_missing_target() {
        "255:1 255:1 P 8".parse::<SnapshotTargetParams>().unwrap();
    }

    #[test]
    #[should_panic]
    fn test_snapshot_target_params_wrong_persitent_flag() {
        "snapshot 255:1 255:1 R 8"
            .parse::<SnapshotTargetParams>()
            .unwrap();
    }
}
