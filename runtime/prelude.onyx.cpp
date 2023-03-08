#include "runtime/prelude.onyx.hpp"
class String {
public:
String* create() {
{
this.data = data;
this.length = data.size();
}
}
size_t length() {
return this.length;
}
private:
uint8_t* data;
size_t length;
};
Result<int32_t, char*> foo(){
return Ok<i32>{42};
}
