import logging

formatter = logging.Formatter(
	fmt='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
	datefmt='%Y-%m-%d %H:%M:%S')

stream_handler = logging.StreamHandler()
stream_handler.setLevel(logging.DEBUG)
stream_handler.setFormatter(formatter)

file_handler = logging.FileHandler('boss.log', mode='a')
file_handler.setLevel(logging.DEBUG)
file_handler.setFormatter(formatter)

def getLogger(name):
	logger = logging.getLogger(name)
	logger.addHandler(stream_handler)
	logger.addHandler(file_handler)
	logger.setLevel(logging.DEBUG)
	return logger

