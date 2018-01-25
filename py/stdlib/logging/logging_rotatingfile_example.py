# -*- encoding: utf-8 -*-
# 为防止 log 文件越来越大，可以在 basicConfig() 中设置 filemode = 'w'，让每次 application 启动，都创建新的 log 文件
# 更好的做法，是使用 RotatingFileHandler
#
# RotatingFileHandler 可以设置一个 log 文件的大小(maxBytes)，以及允许存在几个文件(backupCount)

import glob
import logging
import logging.handlers

LOG_FILENAME = 'logging_rotatingfile_example.out'

# Set up a specific logger with our desired output level
my_logger = logging.getLogger('MyLogger')
my_logger.setLevel(logging.DEBUG)

# Add the log message handler to the logger
handler = logging.handlers.RotatingFileHandler(LOG_FILENAME, maxBytes=20, backupCount=5)
my_logger.addHandler(handler)

# Log some messages
for i in range(100):
	my_logger.debug('i = %i' % i)

# See what files are created
logfiles = glob.glob('%s*' % LOG_FILENAME)
for filename in logfiles:
	print filename

