use std::cell::RefCell;
use std::collections::hash_map::Entry;
use std::collections::HashMap;

pub struct Sources {
    srcs: RefCell<HashMap<String, String>>,
}

impl<'me> Sources {
    pub fn new() -> Self {
        Sources {
            srcs: RefCell::new(HashMap::new()),
        }
    }
    /** @return (filename: &str, file contents: &str) */
    pub fn get_src(self: &'me Self, filename: &str) -> (&'me str, &'me str) {
        macro_rules! ref_str_fake_lifetime {
            ($a: expr) => {
                unsafe { &*($a.as_str() as *const str) }
            };
        }
        match self.srcs.borrow_mut().entry(filename.to_string()) {
            Entry::Occupied(o) => (
                ref_str_fake_lifetime!(o.key()),
                ref_str_fake_lifetime!(o.get()),
            ),
            Entry::Vacant(v) => {
                println!("reading {}", filename);
                let src: String = match std::fs::read_to_string(filename) {
                    Ok(r) => r,
                    Err(e) => {
                        eprintln!("Error reading '{}': {}", filename, e);
                        std::process::exit(-1);
                    }
                };
                let k = ref_str_fake_lifetime!(v.key());
                let r = ref_str_fake_lifetime!(src);
                v.insert(src);
                (k, r)
            }
        }
    }
}
