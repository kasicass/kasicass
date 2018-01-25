# -*- encoding: utf-8 -*-
# 
# CRITICAL 50
# ERROR    40
# WARNING  30
# INFO     20
# DEBUG    10
# UNSET    0

import logging
import sys

LEVELS = {
	'debug'   : logging.DEBUG,
	'info'    : logging.INFO,
	'warning' : logging.WARNING,
	'error'   : logging.ERROR,
	'critical': logging.CRITICAL,
}

if len(sys.argv) > 1:
	level_name = sys.argv[1]
	level = LEVELS.get(level_name, logging.NOTSET)
	logging.basicConfig(level=level)

logging.debug('This is a debug message')
logging.info('This is an info message')
logging.warning('This is a warning message')
logging.error('This is an error message')
logging.critical('This is a criticial error message')

