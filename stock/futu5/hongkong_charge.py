import sys
import math

if __name__ == '__main__':
	hkd = int(sys.argv[1])
	commission = hkd * 0.01 * 0.05              # 0.05%
	if commission < 50: commission = 50
	stamp = math.ceil(hkd * 0.01 * 0.1)         # 0.1%
	transaction_levy = hkd * 0.01 * 0.0027      # 0.0027%
	trading_fee = hkd * 0.01 * 0.005            # 0.005%
	clearing_fee = max(hkd * 0.01 * 0.005, 5.5) # 0.005%

	print 'hkd             :', hkd
	print 'commission      :', commission
	print 'stamp           :', stamp
	print 'transaction_levy:', transaction_levy
	print 'trading_fee     :', trading_fee
	print 'clearing_fee    :', clearing_fee
	print 'total           :', hkd + commission + stamp + transaction_levy + trading_fee + clearing_fee

