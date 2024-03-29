#undef _$header
#undef _$footer
#undef __$quote
#undef _$quote
#undef _$integer
#undef _$logical
#undef _$double
#undef _$integerarray
#undef _$logicalarray
#undef _$doublearray

#define __$quote(x) #x
#define _$quote(x) __$quote(x)

call xml_ftag(info,'fam','open')

#define _$header
#define _$footer
#define _$integer(x,y)        call ftxmlwrite(info,fam(i)% x,#x)
#define _$logical(x,y)        call ftxmlwrite(info,fam(i)% x,#x)
#define _$double(x,y)         call ftxmlwrite(info,fam(i)% x,#x)
#define _$integerarray(x,y,z) call ftxmlwrite(info,fam(i)% x,#x)
#define _$logicalarray(x,y,z) call ftxmlwrite(info,fam(i)% x,#x)
#define _$doublearray(x,y,z)  call ftxmlwrite(info,fam(i)% x,#x)

#include 'includes/fam_t.inc'

#undef _$integer
#undef _$logical
#undef _$double
#undef _$integerarray
#undef _$logicalarray
#undef _$doublearray

#define _$integer(x,y)        call ftxmlwrite(info,fam(i)%ad(1)% x,#x)
#define _$logical(x,y)        call ftxmlwrite(info,fam(i)%ad(1)% x,#x)
#define _$double(x,y)         call ftxmlwrite(info,fam(i)%ad(1)% x,#x)
#define _$integerarray(x,y,z) call ftxmlwrite(info,fam(i)%ad(1)% x,#x)
#define _$logicalarray(x,y,z) call ftxmlwrite(info,fam(i)%ad(1)% x,#x)
#define _$doublearray(x,y,z)  call ftxmlwrite(info,fam(i)%ad(1)% x,#x)

call xml_ftag(info,'ad1','open')

#include 'includes/famad_t.inc'

call xml_ftag(info,'ad1','close')

#undef _$integer
#undef _$logical
#undef _$double
#undef _$integerarray
#undef _$logicalarray
#undef _$doublearray

#define _$integer(x,y)        call ftxmlwrite(info,fam(i)%ad(2)% x,#x)
#define _$logical(x,y)        call ftxmlwrite(info,fam(i)%ad(2)% x,#x)
#define _$double(x,y)         call ftxmlwrite(info,fam(i)%ad(2)% x,#x)
#define _$integerarray(x,y,z) call ftxmlwrite(info,fam(i)%ad(2)% x,#x)
#define _$logicalarray(x,y,z) call ftxmlwrite(info,fam(i)%ad(2)% x,#x)
#define _$doublearray(x,y,z)  call ftxmlwrite(info,fam(i)%ad(2)% x,#x)

call xml_ftag(info,'ad2','open')

#include 'includes/famad_t.inc'

call xml_ftag(info,'ad2','close')

#undef _$header
#undef _$footer
#undef __$quote
#undef _$quote
#undef _$integer
#undef _$logical
#undef _$double
#undef _$integerarray
#undef _$logicalarray
#undef _$doublearray

call xml_ftag(info,'fam','close')

attribs(2,1) = sysname(i)
call xml_ftag(info,'net','open',attribs)

#define _$header
#define _$footer

#define _$integer(x,y,z)        call ftxmlwrite(info,net(i)%tu% x,#x)
#define _$logical(x,y,z)        call ftxmlwrite(info,net(i)%tu% x,#x)
#define _$double(x,y,z)         call ftxmlwrite(info,net(i)%tu% x,#x)

call xml_ftag(info,'nettu','open')

#include 'includes/nettu_t.inc'

call xml_ftag(info,'nettu','close')

#undef _$header
#undef _$footer
#undef __$quote
#undef _$quote
#undef _$integer
#undef _$logical
#undef _$double
#undef _$integerarray
#undef _$logicalarray
#undef _$doublearray

#define _$header
#define _$footer

#define _$integer(x,y,z)        call ftxmlwrite(info,net(i)%ad(1)% x,#x)
#define _$logical(x,y,z)        call ftxmlwrite(info,net(i)%ad(1)% x,#x)
#define _$double(x,y,z)         call ftxmlwrite(info,net(i)%ad(1)% x,#x)

call xml_ftag(info,'netad1','open')

#include 'includes/netad_t.inc'

call xml_ftag(info,'netad1','close')

#undef _$header
#undef _$footer
#undef __$quote
#undef _$quote
#undef _$integer
#undef _$logical
#undef _$double
#undef _$integerarray
#undef _$logicalarray
#undef _$doublearray

#define _$header
#define _$footer

#define _$integer(x,y,z)        call ftxmlwrite(info,net(i)%ad(2)% x,#x)
#define _$logical(x,y,z)        call ftxmlwrite(info,net(i)%ad(2)% x,#x)
#define _$double(x,y,z)         call ftxmlwrite(info,net(i)%ad(2)% x,#x)

call xml_ftag(info,'netad2','open')

#include 'includes/netad_t.inc'

call xml_ftag(info,'netad2','close')

#undef _$header
#undef _$footer
#undef __$quote
#undef _$quote
#undef _$integer
#undef _$logical
#undef _$double
#undef _$integerarray
#undef _$logicalarray
#undef _$doublearray

call xml_ftag(info,'net','close')
