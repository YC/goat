#[allow(dead_code)]
enum RegEx {
    Or(Box<RegEx>, Box<RegEx>),
    Character(char),
    Literal(String),
    RepeatMany(Box<RegEx>),
}
