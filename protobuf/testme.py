# protoc -I=$SRC_DIR --python_out=$DST_DIR $SRC_DIR/addressbook.proto

from addressbook_pb2 import Person
a = Person()
a.name = 'phay'
a.id = 100
data = a.SerializeToString()

b = Person()
b.ParseFromString(data)
print b

