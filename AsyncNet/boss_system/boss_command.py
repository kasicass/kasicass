import time
import msgpack
import boss_config
import boss_logger
import boss_network

class BossCommand(boss_network.BaseClient):
	def __init__(self):
		super(BossCommand, self).__init__()
		self.logger = boss_logger.getLogger('BossCommand')

	def handle_connected(self):
		self.logger.info('connected %d', self.hid())

		data = ('HelloStub', 'sayHello', ())
		data = msgpack.packb(data)
		self.send(data)

	def handle_disconnected(self):
		self.logger.info('disconnected %d', self.hid())

	def handle_data(self, data):
		self.logger.info('recv %s', data)

if __name__ == '__main__':
	client = BossCommand()
	client.new_connect(boss_config.BOSS_ENDPOINT)

	exit_time = time.time() + 5  # 5s later
	while exit_time > time.time():
		client.run(0.1)
		time.sleep(0.1)

