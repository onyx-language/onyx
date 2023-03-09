#include <stdint.h>
#include "prelude.onyx.hpp"
template<typename T>
Array<T>* Array<T>::create(T* data) {
return new Array<T>(data, sizeof(data));
}
String* String::create(uint8_t* data) {
return new String(data, sizeof(data));
}
size_t String::length() {
return this->m_length;
}
Result<int32_t, String> foo(){
return Ok<int32_t>{42};
}
int32_t main(int32_t argc, char** argv){
return 0;
}
