# -*- encoding: utf-8 -*-

# 聚福盈门
# http://www.citic-prudential.com.cn/productinfo.aspx?id=21504

import sys

PAY_PER_YEAR = 36055
YEARS = 20

if __name__ == '__main__':
    RATE = 1.0 + int(sys.argv[1]) / 100.0  # 年投资回报率
    END_YEAR = int(sys.argv[2])            # 投资多少年
    total = 0
    for i in xrange(0, YEARS):
        total += PAY_PER_YEAR
        total *= RATE
    
    if END_YEAR > YEARS:
        for i in xrange(0, END_YEAR - YEARS):
            total *= RATE
            
    cnt = min(YEARS, END_YEAR)
    pay = int(PAY_PER_YEAR * cnt)
    print 'pay: %d(%dw)' % (pay, pay/10000)
    print 'rate: %d%%' % (int(sys.argv[1]))
    print 'total: %d(%dw)' % (total, total/10000)