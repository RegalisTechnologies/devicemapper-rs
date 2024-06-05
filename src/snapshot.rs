// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use crate::shared::{device_create, device_exists, device_match, parse_device};
use crate::{
    DevId, Device, DeviceInfo, DmDevice, DmError, DmName, DmOptions, DmResult, DmUuid, ErrorEnum,
    Sectors, TargetLine, TargetParams, TargetTable, TargetTypeBuf, DM,
};

use std::fmt::Display;

const SNAPSHOT_TARGET_NAME: &str = "snapshot";

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SnapshotDevTargetTable {
    pub table: Vec<TargetLine<SnapshotTargetParams>>,
}

#[derive(Debug)]
pub struct SnapshotDev {
    dev_info: Box<DeviceInfo>,
    table: SnapshotDevTargetTable,
}

impl SnapshotDev {
    pub fn setup(
        dm: &DM,
        name: &DmName,
        uuid: Option<&DmUuid>,
        start: Sectors,
        length: Sectors,
        params: SnapshotTargetParams,
    ) -> DmResult<SnapshotDev> {
        let table = SnapshotDevTargetTable::new(start, length, params);
        let dev = if device_exists(dm, name)? {
            let dev_info = dm.device_info(&DevId::Name(name))?;
            let dev = SnapshotDev {
                dev_info: Box::new(dev_info),
                table,
            };
            device_match::<SnapshotDevTargetTable, SnapshotDev>(dm, &dev, uuid)?;
            dev
        } else {
            let dev_info = device_create(dm, name, uuid, &table, DmOptions::default())?;
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

impl SnapshotDevTargetTable {
    fn new(start: Sectors, length: Sectors, params: SnapshotTargetParams) -> Self {
        Self {
            table: vec![TargetLine::new(start, length, params)],
        }
    }
}

impl Display for SnapshotDevTargetTable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Hi")
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
enum SnapshotPersistent {
    Persistent,
    NonPersistent,
    Overflow,
}

impl Display for SnapshotPersistent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SnapshotPersistent::Persistent => f.write_str("P"),
            SnapshotPersistent::NonPersistent => f.write_str("N"),
            SnapshotPersistent::Overflow => f.write_str("PO"),
        }
    }
}

use std::str::FromStr;
use std::str::Split;

impl TargetParams for SnapshotTargetParams {
    fn param_str(&self) -> String {
        format!(
            "{} {} {} {}",
            self.origin, self.cow_device, self.persistent, self.chunksize
        )
    }

    fn target_type(&self) -> TargetTypeBuf {
        TargetTypeBuf::new(SNAPSHOT_TARGET_NAME.into()).expect("SNAPSHOT_TARGET_NAME is valid")
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

//TODO: implement support for features
//Optional features:
// discard_zeroes_cow - a discard issued to the snapshot device that maps to entire chunks to will
// zero the corresponding exception(s) in the snapshot’s exception store.
// discard_passdown_origin - a discard to the snapshot device is passed down to the snapshot-origin’s underlying device. This
// doesn’t cause copy-out to the snapshot exception store because the snapshot-origin target is
// bypassed.
// ref: https://www.kernel.org/doc/html/latest/admin-guide/device-mapper/snapshot.html
//
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SnapshotTargetParams {
    origin: Device,
    cow_device: Device,
    persistent: SnapshotPersistent,
    chunksize: usize,
}

impl Display for SnapshotTargetParams {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", SNAPSHOT_TARGET_NAME, self.param_str())
    }
}

fn get_next_token<'a>(split: &mut Split<'a, char>, token_name: &str) -> DmResult<&'a str> {
    split.next().ok_or(DmError::Dm(
        ErrorEnum::Invalid,
        format!("No {} string in params string", token_name),
    ))
}

impl FromStr for SnapshotTargetParams {
    type Err = DmError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut split = s.split(' ');
        let snapshot_string = get_next_token(&mut split, "snapshot")?;
        if snapshot_string != "snapshot" {
            return Err(DmError::Dm(
                ErrorEnum::Invalid,
                format!("expecting 'snapshot' string, got '{}'", snapshot_string),
            ));
        }

        let device_string = get_next_token(&mut split, "<origin>")?;
        let origin = parse_device(device_string, "block device for snapshot origin")?;

        let cow_string = get_next_token(&mut split, "<COW device>")?;
        let cow_device = parse_device(cow_string, "block device for snapshot COW device")?;
        let persitent_string = get_next_token(&mut split, "<persistent?>")?;
        let persistent: SnapshotPersistent = match persitent_string {
            "P" => Ok(SnapshotPersistent::Persistent),
            "N" => Ok(SnapshotPersistent::NonPersistent),
            "PO" => Ok(SnapshotPersistent::Overflow),
            _ => Err(DmError::Dm(
                ErrorEnum::Invalid,
                format!(
                    "Invalid persistent flag value: {}, expected one of: 'P', 'N', 'PO'",
                    persitent_string
                ),
            )),
        }?;

        let chunksize_string = get_next_token(&mut split, "<chunksize>")?;
        let chunksize = usize::from_str(chunksize_string).map_err(|err| {
            DmError::Dm(
                ErrorEnum::Invalid,
                format!("Invalid <chunksize> value: {}, {}", chunksize_string, err),
            )
        })?;

        Ok(SnapshotTargetParams {
            origin,
            cow_device,
            persistent,
            chunksize,
        })
    }
}
