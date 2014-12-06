import sys
from PyQt4.QtCore import *
from PyQt4.QtGui import *

class EditDlg(QDialog):
	def __init__(self, name):
		super(EditDlg, self).__init__()

		self.lblTitle = QLabel(name)
		self.edtText  = QLineEdit()
		self.btnBox   = QDialogButtonBox(QDialogButtonBox.Ok | QDialogButtonBox.Close)

		vlayout = QVBoxLayout()
		vlayout.addWidget(self.lblTitle)
		vlayout.addWidget(self.edtText)
		vlayout.addWidget(self.btnBox)

		self.setLayout(vlayout)
		self.setWindowTitle(name)

		self.connect(self.btnBox.button(QDialogButtonBox.Ok), SIGNAL("clicked()"), self, SLOT("accept()"))
		self.connect(self.btnBox.button(QDialogButtonBox.Close), SIGNAL("clicked()"), self, SLOT("reject()"))


class StringListDlg(QDialog):
	def __init__(self, name, strList):
		super(StringListDlg, self).__init__()

		self.listStrings = QListWidget()
		self.listStrings.addItems(strList)

		self.btnAdd    = QPushButton("&Add...")
		self.btnEdit   = QPushButton("&Edit...")
		self.btnRemove = QPushButton("&Remove...")
		self.btnUp     = QPushButton("&Up")
		self.btnDown   = QPushButton("&Down")
		self.btnSort   = QPushButton("&Sort")
		self.btnClose  = QPushButton("&Close")

		vlayout = QVBoxLayout()
		vlayout.addWidget(self.btnAdd)
		vlayout.addWidget(self.btnEdit)
		vlayout.addWidget(self.btnRemove)
		vlayout.addWidget(self.btnUp)
		vlayout.addWidget(self.btnDown)
		vlayout.addWidget(self.btnSort)
		vlayout.addWidget(self.btnClose)

		hlayout = QHBoxLayout()
		hlayout.addWidget(self.listStrings)
		hlayout.addLayout(vlayout)

		self.setLayout(hlayout)
		self.setWindowTitle("Edit " + name + " List")

		self.connect(self.btnAdd, SIGNAL('clicked()'), self.addItem)
		self.connect(self.btnEdit, SIGNAL('clicked()'), self.editItem)
		self.connect(self.btnRemove, SIGNAL('clicked()'), self.removeItem)
		self.connect(self.btnUp, SIGNAL('clicked()'), self.itemMoveUp)
		self.connect(self.btnDown, SIGNAL('clicked()'), self.itemMoveDown)
		self.connect(self.btnSort, SIGNAL('clicked()'), self.listStrings.sortItems)
		self.connect(self.btnClose, SIGNAL('clicked()'), self.reject)

	def addItem(self):
		dlg = EditDlg('Add Fruit')
		if dlg.exec_():
			text = dlg.edtText.text()
			self.listStrings.addItem(text)

	def editItem(self):
		item = self.listStrings.currentItem()
		dlg = EditDlg('Edit Fruit')
		dlg.edtText.setText(item.text())
		if dlg.exec_():
			text = dlg.edtText.text()
			item.setText(text)

	def removeItem(self):
		dlg = EditDlg('Remove Fruit')
		if dlg.exec_():
			text = dlg.edtText.text()
			self.listStrings.addItem(text)

	def itemMoveUp(self):
		row = self.listStrings.currentRow()
		if row > 0:
			item = self.listStrings.takeItem(row)
			self.listStrings.insertItem(row-1, item)
			self.listStrings.setCurrentRow(row-1)

	def itemMoveDown(self):
		row = self.listStrings.currentRow()
		if row < self.listStrings.count() - 1:
			item = self.listStrings.takeItem(row)
			self.listStrings.insertItem(row+1, item)
			self.listStrings.setCurrentRow(row+1)

	def accept(self):
		self.stringlist = []
		n = self.listStrings.count()
		for i in xrange(0, n):
			text = self.listStrings.item(i).text()
			self.stringlist.append(text)
		QDialog.accept(self)

	def reject(self):
		self.accept()


if __name__ == '__main__':
	fruit = ["Banana", "Apple", "Mango", "Kiwi", "Lemon", "Raspberry"]
	app = QApplication(sys.argv)
	form = StringListDlg("Fruit", fruit)
	form.exec_()
	print "\n".join([unicode(x) for x in form.stringlist])
