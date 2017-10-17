# python -m pip install flask-script
# python hello.py runserver --host 127.0.0.1

from flask import Flask
from flask.ext.script import Manager

app = Flask(__name__)
manager = Manager(app)

@app.route("/")
def hello():
	return 'Hello World'

if __name__ == '__main__':
	manager.run()

