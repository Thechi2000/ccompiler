use std::{io::Read, iter::Peekable};

pub type Token = String;

pub struct Tokeniser<I: Iterator<Item = u8>>(Peekable<I>);

pub struct IterReader<R: Read>(R);

impl<R: Read> IterReader<R> {
    pub fn new(r: R) -> Self {
        return Self(r);
    }
}

impl<R: Read> Iterator for IterReader<R> {
    type Item = u8;

    fn next(&mut self) -> Option<Self::Item> {
        let mut buf = [0];
        self.0.read_exact(&mut buf).ok()?;
        return Some(buf[0]);
    }
}

const STANDALONES: [&str; 42] = [
    "{", "}", "[", "]", "(", "]", "+", "-", "*", "/", "%", "++", "--", "|", "&", "<<", ">>", "^",
    "=", "+=", "-=", "*=", "/=", "%=", "&=", "|=", "^=", ">>=", "<<=", "==", "!=", ">", "<", ">=",
    "<=", "&&", "||", "!", "~", ",", ";", ":",
];

impl<I: Iterator<Item = u8>> Tokeniser<I> {
    pub fn new(it: I) -> Self {
        return Tokeniser(it.peekable());
    }
}

impl<I: Iterator<Item = u8>> Iterator for Tokeniser<I> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(..) = self.0.next_if(|c| c.is_ascii_whitespace()) {}
        let next = self.0.next()? as char;

        let standalones = STANDALONES
            .iter()
            .filter(|s| s.starts_with(next))
            .collect::<Vec<_>>();

        if !standalones.is_empty() {
            let mut acc = String::from_iter(&[next]);

            loop {
                if let Some(next) = self.0.peek() {
                    let mut new_acc = acc.clone();
                    new_acc.push(*next as char);

                    if !standalones.iter().any(|s| s.starts_with(new_acc.as_str())) {
                        return Some(acc);
                    } else {
                        acc.push(*next as char);
                        self.0.next();
                    }
                } else {
                    return Some(acc);
                }
            }
        } else {
            let mut s = String::from_iter(&[next]);

            while self.0.peek().is_some_and(|c| {
                !c.is_ascii_whitespace() && !STANDALONES.iter().any(|s| s.starts_with(*c as char))
            }) {
                let c = self.0.next()? as char;
                s.push(c);
            }

            Some(s)
        }
    }
}
