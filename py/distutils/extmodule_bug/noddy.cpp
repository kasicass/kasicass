#include <Python.h>

typedef struct {
  PyObject_HEAD
} noddy_NoddyObject;

static PyTypeObject noddy_NoddyType = {
  PyObject_HEAD_INIT(&PyType_Type)
  0,                         /*ob_size*/
  "noddy.Noddy",             /*tp_name*/
  sizeof(noddy_NoddyObject), /*tp_basicsize*/
  0,                         /*tp_itemsize*/
  0,                         /*tp_dealloc*/
  0,                         /*tp_print*/
  0,                         /*tp_getattr*/
  0,                         /*tp_setattr*/
  0,                         /*tp_compare*/
  0,                         /*tp_repr*/
  0,                         /*tp_as_number*/
  0,                         /*tp_as_sequence*/
  0,                         /*tp_as_mapping*/
  0,                         /*tp_hash */
  0,                         /*tp_call*/
  0,                         /*tp_str*/
  0,                         /*tp_getattro*/
  0,                         /*tp_setattro*/
  0,                         /*tp_as_buffer*/
  Py_TPFLAGS_DEFAULT,        /*tp_flags*/
  "Noddy objects",           /* tp_doc */
};

static PyMethodDef noddy_methods[] = {
  {NULL}  /* Sentinel */
};

PyMODINIT_FUNC
initnoddy(void)
{
  PyObject *m;
  noddy_NoddyType.tp_new = PyType_GenericNew;

  if (PyType_Ready(&noddy_NoddyType) < 0)
    return;

  Py_INCREF(&noddy_NoddyType);
  m = Py_InitModule3("noddy", noddy_methods, "example module");
  PyModule_AddObject(m, "Noddy", (PyObject*)&noddy_NoddyType);
}

