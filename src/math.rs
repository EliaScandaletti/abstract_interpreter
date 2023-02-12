pub fn lfp<F, T>(bot: T, l: F) -> T
where
    T: Clone + std::cmp::PartialEq,
    F: Fn(T) -> T,
{
    let mut x = bot;
    let mut y = l(x.clone());
    while x != y {
        x = y.clone();
        y = l(x.clone());
    }
    x
}
