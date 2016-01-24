import sys

if __name__ == '__main__':
	usd = int(sys.argv[1])
	stock_price = int(sys.argv[2])
	N = usd / stock_price
	commission = 5
	clearing_fee = max(N * 0.003, 3)
	sec_fee = max(usd*0.01*0.00184, 0.01)
	taf = N*0.000119
	if taf < 0.01: taf = 0.01
	if taf > 5.95: taf = 5.95

	print 'usd             :', usd
	print 'stock_price     :', stock_price
	print 'stock N         :', N
	print 'commission      :', commission
	print 'clearing_fee    :', clearing_fee
	print 'sec_fee         :', sec_fee
	print 'taf             :', taf
	print 'total           :', usd + commission + clearing_fee + sec_fee + taf

