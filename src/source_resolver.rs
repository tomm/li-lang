use std::cell::RefCell;
use std::collections::HashMap;
use std::collections::hash_map::Entry;
    //let buf: String = match std::fs::read_to_string("hello.txt") {

pub struct Sources {
    srcs: RefCell<HashMap<String, String>>
}

impl<'me> Sources {
    pub fn new() -> Self {
        Sources {
            srcs: RefCell::new(HashMap::new())
        }
    }
    /** @return (filename: &str, file contents: &str) */
    pub fn get_src(self: &'me Self, filename: &str) -> (&'me str, &'me str) {
        macro_rules! ref_str_fake_lifetime {
            ($a: expr) => { unsafe { &*($a.as_str() as *const str) } }
        }
        match self.srcs.borrow_mut().entry(filename.to_string()) {
            Entry::Occupied(o) =>
                (ref_str_fake_lifetime!(o.key()), ref_str_fake_lifetime!(o.get())),
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

/*
fn test(src: &mut Sources) -> &str {
    println!("{:?}", src.get_src("test.txt"));
    let s = src.get_src("test.txt");
    println!("{:?}", s);
    s.1
}

fn main() {
    let mut src: Sources = Sources::new();
    println!("blah {}", test(&mut src));
    src.dump();
}
*/

#[cfg(test)]
mod tests {
    use super::*;

    fn aux(src: &Sources) -> &str {
        let s = src.get_src("test.txt");
        println!("{:?}", s);
        s.1
    }

    #[test]
    fn blah() {
        let src: Sources = Sources::new();
        let a = aux(&src);
        let b = aux(&src);
        println!("blah {}", a);
        println!("blah {}", b);
    }
}
