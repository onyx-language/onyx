#include <stdint.h>
#include <variant>
template<typename R> struct Ok {R value;};
template<typename E> struct Err {E value;};
template<typename R, typename E>
using Result = std::variant<Ok<R>, Err<E>>;
template<typename T>
class Array {
public:
~Array<T>() {}
static Array<T>* create(T* data);
private:
Array<T>(T* m_data, size_t m_size) : m_data(m_data), m_size(m_size) {}
T* m_data;
size_t m_size;
};
class String {
public:
~String() {}
static String* create(uint8_t* data);
size_t length();
private:
String(uint8_t* m_data, size_t m_length) : m_data(m_data), m_length(m_length) {}
uint8_t* m_data;
size_t m_length;
};
