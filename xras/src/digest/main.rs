use std::{
    collections::HashMap,
    env,
    error::Error,
    fs::File,
    io::{self, BufReader, Write},
};

use zip::ZipArchive;

use encoding_rs_io::DecodeReaderBytes;

use quick_xml::{events::Event, Reader};

use glob::glob;

use serde::Serialize;
use toml::ser;

const BUF_SIZE: usize = 4096; // 4kb at once

#[derive(Copy, Clone, Debug)]
enum ParserState {
    Between,
    ReadBeatsPerMin,
    ReadLoopCoeff,
}

#[derive(Serialize)]
struct XRNSData {
    pub beats_per_min: f32,
    pub loop_coeff: u8,
}

impl XRNSData {
    pub fn new() -> Self {
        XRNSData {
            beats_per_min: 0.0,
            loop_coeff: 0,
        }
    }
}

struct XRNSParser {
    state: ParserState,
    res: XRNSData,
}

impl XRNSParser {
    pub fn new() -> Self {
        XRNSParser {
            state: ParserState::Between,
            res: XRNSData::new(),
        }
    }

    pub fn result(self) -> XRNSData {
        self.res
    }

    pub fn is_finished(&self) -> bool {
        (self.res.beats_per_min != 0.0) && (self.res.loop_coeff != 0)
    }

    pub fn process(&mut self, ev: Event) -> Result<(), Box<dyn Error>> {
        self.state = match self.state {
            ParserState::Between => match ev {
                Event::Start(e) => match e.name().as_ref() {
                    b"BeatsPerMin" => ParserState::ReadBeatsPerMin,
                    b"LoopCoeff" => ParserState::ReadLoopCoeff,
                    _ => ParserState::Between,
                },
                _ => ParserState::Between,
            },
            ParserState::ReadBeatsPerMin => match ev {
                Event::Text(e) => {
                    self.res.beats_per_min =
                        str::parse::<f32>(&e.unescape().unwrap().into_owned())?;

                    ParserState::Between
                }
                _ => ParserState::ReadBeatsPerMin,
            },
            ParserState::ReadLoopCoeff => match ev {
                Event::Text(e) => {
                    self.res.loop_coeff = str::parse::<u8>(&e.unescape().unwrap().into_owned())?;
                    ParserState::Between
                }
                _ => ParserState::ReadLoopCoeff,
            },
        };
        Ok(())
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    let mut results = HashMap::new();
    for entry in glob("*.xrns")? {
        match entry {
            Ok(path) => {
                let path_scoped = path;
                let zipfile = File::open(&path_scoped)?;
                let mut zip = ZipArchive::new(zipfile)?;
                let xmlfile = zip.by_index(0)?;
                let xmlfile = BufReader::new(DecodeReaderBytes::new(xmlfile));
                let mut reado = Reader::from_reader(xmlfile);
                let mut prodparser = XRNSParser::new();
                let mut buf = Vec::with_capacity(BUF_SIZE);
                while !prodparser.is_finished() {
                    match reado.read_event_into(&mut buf)? {
                        Event::Eof => break,
                        ev => {
                            prodparser.process(ev)?;
                        }
                    }
                    buf.clear();
                }
                let xrns = prodparser.result();
                results.insert(path_scoped, xrns);
            }
            Err(e) => println!("{:?}", e),
        }
    }
    let mut file = File::create("out.toml")?;
    let toml = toml::to_string(&results).unwrap();
    file.write_all(&toml.as_bytes())?;
    Ok(())
}
