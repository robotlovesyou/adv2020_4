use std::collections::HashMap;
use std::fmt::{self, Formatter};
use std::fs::File;
use std::io::{self, Read};
use std::path::Path;
use std::{env, result, str};

use lazy_static::*;
use regex::Regex;

#[derive(Debug)]
struct Error {
    message: String,
}

impl Error {
    fn new(message: String) -> Error {
        Error { message }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

macro_rules! error_from {
    ($kind:path, $friendly_name:literal) => {
        impl From<$kind> for Error {
            fn from(e: $kind) -> Error {
                Error::new(format!("{}: {}", $friendly_name, e))
            }
        }
    };
}

error_from!(io::Error, "io error");

struct OwnedCharIter {
    chars: Vec<char>,
    idx: usize,
}

impl OwnedCharIter {
    fn new(source: String) -> OwnedCharIter {
        OwnedCharIter {
            chars: source.chars().collect(),
            idx: 0,
        }
    }
}

impl Iterator for OwnedCharIter {
    type Item = char;
    fn next(&mut self) -> Option<Self::Item> {
        if let Some(c) = self.chars.get(self.idx) {
            self.idx += 1;
            return Some(*c);
        }
        None
    }
}

trait OwnedChars {
    fn owned_chars(self) -> OwnedCharIter;
}

impl OwnedChars for String {
    fn owned_chars(self) -> OwnedCharIter {
        OwnedCharIter::new(self)
    }
}

type Result<T> = result::Result<T, Error>;

fn open_char_reader<P: AsRef<Path>>(filename: P) -> Result<OwnedCharIter> {
    let file = File::open(filename)?;
    let mut contents = String::new();
    io::BufReader::new(file).read_to_string(&mut contents)?;
    Ok(contents.owned_chars())
}

struct Pair {
    key: String,
    value: String,
}

impl Pair {
    fn new(key: String, value: String) -> Pair {
        Pair { key, value }
    }
}

struct Tokens {
    chars: OwnedCharIter,
}

impl Tokens {
    fn new(chars: OwnedCharIter) -> Tokens {
        Tokens { chars }
    }

    fn parse_pair(&mut self, initial: char) -> Result<Token> {
        let mut key = format!("{}", initial);
        let mut value = String::new();
        loop {
            let c = self.chars.next();
            match c {
                Some(':') => break,
                Some('\n') => return Err(Error::new("newline in key".to_string())),
                None => return Err(Error::new("end of file in key".to_string())),
                Some(c) => key.push(c),
            }
        }
        while let Some(c) = self.chars.next() {
            match c {
                '\n' | ' ' => break,
                ':' => return Err(Error::new(": in value".to_string())),
                _ => value.push(c),
            }
        }
        Ok(Token::Pair(Pair::new(key, value)))
    }
}

enum Token {
    Pair(Pair),
    Break,
}

impl Iterator for Tokens {
    type Item = Result<Token>;

    fn next(&mut self) -> Option<Result<Token>> {
        self.chars.next().map(|c| match c {
            '\n' => Ok(Token::Break),
            _ => self.parse_pair(c),
        })
    }
}

trait IntoTokens {
    fn into_tokens(self) -> Tokens;
}

impl IntoTokens for OwnedCharIter {
    fn into_tokens(self) -> Tokens {
        Tokens::new(self)
    }
}

struct Passport {
    pairs: HashMap<String, String>,
}

const CID: &str = "cid";
const BYR: &str = "byr";
const IYR: &str = "iyr";
const EYR: &str = "eyr";
const HGT: &str = "hgt";
const HCL: &str = "hcl";
const ECL: &str = "ecl";
const PID: &str = "pid";

lazy_static! {
    static ref REQUIRED: Vec<String> = vec![
        BYR.to_string(),
        IYR.to_string(),
        EYR.to_string(),
        HGT.to_string(),
        HCL.to_string(),
        ECL.to_string(),
        PID.to_string(),
    ];
    static ref HGT_REGEX: Regex =
        Regex::new(r"^(?P<amount>\d+)(?P<unit>cm|in)$").expect("invalid height regex");
    static ref HCL_REGEX: Regex = Regex::new(r"^#[a-f0-9]{6}$").expect("invalid hair color regex");
    static ref ECL_REGEX: Regex =
        Regex::new(r"^(?:amb|blu|brn|gry|grn|hzl|oth)$").expect("invalid eye color regex");
    static ref PID_REGEX: Regex = Regex::new(r"^\d{9}$").expect("invalid passport id regex");
}

impl Passport {
    fn new(pairs: HashMap<String, String>) -> Passport {
        Passport { pairs }
    }

    fn contains_required_fields(&self) -> bool {
        self.contains_passport_required_fields() || self.contains_north_pole_required_fields()
    }

    fn contains_passport_required_fields(&self) -> bool {
        self.pairs.len() == REQUIRED.len() + 1
            && self.contains_min_fields()
            && self.contains_cid_field()
    }

    fn contains_north_pole_required_fields(&self) -> bool {
        self.pairs.len() == REQUIRED.len() && self.contains_min_fields()
    }

    fn contains_min_fields(&self) -> bool {
        REQUIRED.iter().all(|key| self.pairs.contains_key(key))
    }

    fn contains_cid_field(&self) -> bool {
        self.pairs.contains_key(CID)
    }

    fn is_valid(&self) -> bool {
        let valid = self.is_valid_byr()
            && self.is_valid_iyr()
            && self.is_valid_eyr()
            && self.is_valid_hgt()
            && self.is_valid_hcl()
            && self.is_valid_ecl()
            && self.is_valid_pid();
        if valid {
            self._print()
        }
        valid
    }

    fn is_valid_byr(&self) -> bool {
        self.value_in_valid_u32_range(BYR, 1920, 2002)
    }

    fn is_valid_iyr(&self) -> bool {
        self.value_in_valid_u32_range(IYR, 2010, 2020)
    }

    fn is_valid_eyr(&self) -> bool {
        self.value_in_valid_u32_range(EYR, 2020, 2030)
    }

    fn is_valid_hgt(&self) -> bool {
        self.pairs
            .get(HGT)
            .and_then(|value| HGT_REGEX.captures(value))
            .and_then(|captures| {
                captures
                    .name("amount")
                    .map(|height| (captures, height.as_str().to_string()))
            })
            .and_then(|(captures, height)| {
                captures
                    .name("unit")
                    .map(|unit| (height, unit.as_str().to_string()))
            })
            .and_then(|(height, unit)| {
                height.parse::<u32>().ok().map(|h| {
                    (unit == "cm" && h >= 150 && h <= 193) || (unit == "in" && h >= 59 && h <= 76)
                })
            })
            .unwrap_or(false)
    }

    fn is_valid_hcl(&self) -> bool {
        is_valid_match(self.pairs.get(HCL), &HCL_REGEX)
    }

    fn is_valid_ecl(&self) -> bool {
        is_valid_match(self.pairs.get(ECL), &ECL_REGEX)
    }

    fn is_valid_pid(&self) -> bool {
        is_valid_match(self.pairs.get(PID), &PID_REGEX)
    }

    fn value_in_valid_u32_range(&self, key: &str, from: u32, to: u32) -> bool {
        self.pairs
            .get(key)
            .and_then(|value| value.parse::<u32>().ok())
            .map_or(false, |y| y >= from && y <= to)
    }

    fn _print(&self) {
        let out = self
            .pairs
            .iter()
            .map(|(k, v)| format!("{}:{}", k, v))
            .collect::<Vec<String>>()
            .join(" ");
        println!("{}", out);
    }
}

fn is_valid_match(value: Option<&String>, regex: &Regex) -> bool {
    value.map(|v| regex.is_match(v)).unwrap_or(false)
}

struct Passports {
    tokens: Tokens,
}

impl Passports {
    fn new(tokens: Tokens) -> Passports {
        Passports { tokens }
    }
}

impl Iterator for Passports {
    type Item = Passport;

    fn next(&mut self) -> Option<Self::Item> {
        let mut pairs: HashMap<String, String> = HashMap::new();
        loop {
            match self.tokens.next() {
                Some(Ok(Token::Pair(p))) => {
                    pairs.insert(p.key, p.value);
                }
                Some(Ok(Token::Break)) | None if !pairs.is_empty() => {
                    return Some(Passport::new(pairs));
                }
                // ignore extra line breaks
                Some(Ok(Token::Break)) => (),
                Some(Err(e)) => panic!("card error: {}", e),
                None => break,
            }
        }
        None
    }
}

trait IntoPassports {
    fn into_passports(self) -> Passports;
}

impl IntoPassports for Tokens {
    fn into_passports(self) -> Passports {
        Passports::new(self)
    }
}

fn main() -> Result<()> {
    let args = env::args().collect::<Vec<String>>();
    if args.len() > 1 {
        let passports: Vec<Passport> = open_char_reader(&args[1])?
            .into_tokens()
            .into_passports()
            .collect();

        let num_required = passports
            .iter()
            .filter(|p| p.contains_required_fields())
            .count();

        let num_valid = passports
            .iter()
            .filter(|p| p.contains_required_fields() && p.is_valid())
            .count();

        println!(
            "There are {} passports with the required fields",
            num_required
        );
        println!("There are {} valid passports", num_valid);
        Ok(())
    } else {
        panic!("input filename is required");
    }
}
