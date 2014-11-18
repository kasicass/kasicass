import sys
from PyQt4.QtCore import *
from PyQt4.QtGui import *

class MyForm(QDialog):
	def __init__(self, parent=None):
		super(MyForm, self).__init__(parent)

		labelPrincipal = QLabel('Principal:')
		self.spinPrincipal = QDoubleSpinBox()
		self.spinPrincipal.setRange(1.00, 100000000.00)
		self.spinPrincipal.setValue(2000)
		self.spinPrincipal.setPrefix('$ ')
		labelRate = QLabel('Rate:')
		self.spinRate = QDoubleSpinBox()
		self.spinRate.setRange(0.00, 100.00)
		#self.spinRate.setValue(5.25)
		self.spinRate.setSuffix(' %')
		labelYears = QLabel('Years:')
		self.comboYears = QComboBox()
		self.comboYears.addItems(('1 Years', '2 Years', '3 Years'))
		labelAmount = QLabel('Amount:')
		self.labelAmountValue = QLabel('')
		
		grid = QGridLayout()
		grid.addWidget(labelPrincipal, 0, 0)
		grid.addWidget(self.spinPrincipal, 0, 1)
		grid.addWidget(labelRate, 1, 0)
		grid.addWidget(self.spinRate, 1, 1)
		grid.addWidget(labelYears, 2, 0)
		grid.addWidget(self.comboYears, 2, 1)
		grid.addWidget(labelAmount, 3, 0)
		grid.addWidget(self.labelAmountValue, 3, 1)
		self.setLayout(grid)

		self.connect(self.spinPrincipal, SIGNAL('valueChanged(double)'), self.updateUi)
		self.connect(self.spinRate, SIGNAL('valueChanged(double)'), self.updateUi)
		self.connect(self.comboYears, SIGNAL('currentIndexChanged(int)'), self.updateUi)
		self.spinRate.setValue(5.25)

	def updateUi(self):
		years = self.comboYears.currentIndex() + 1
		principal = self.spinPrincipal.value()
		rate = self.spinRate.value()
		amount = principal * ((1+(rate/100.0))**years)
		self.labelAmountValue.setText('$ %.02f' % amount)

		self.setWindowTitle('Interest')


app = QApplication(sys.argv)
form = MyForm()
form.show()
app.exec_()

