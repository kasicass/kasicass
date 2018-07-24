import sys
import pandas

if __name__ == '__main__':
	xls_filename = sys.argv[1]

	xls = pandas.ExcelFile(xls_filename)
	sheet = xls.parse('sheet1')
	sheet.to_csv('sheet1.csv', encoding='gbk')

	sheet = xls.parse('sheet2')
	sheet.to_csv('sheet2.csv', encoding='gbk')

