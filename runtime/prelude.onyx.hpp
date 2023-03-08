#include <variant>
template<typename R> struct Ok {R value};
template<typename E> struct Err {E value};
template<typename R, typename E>
using Result = std::variant<Ok<R>, Err<E>>;
