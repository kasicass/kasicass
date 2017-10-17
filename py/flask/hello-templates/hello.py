# python -m pip install flask-script
# python hello.py runserver --host 127.0.0.1

from flask import Flask, render_template
from flask.ext.script import Manager

app = Flask(__name__)
manager = Manager(app)

@app.route("/")
def index():
	return render_template('index.html')

@app.route("/user/<name>")
def user(name):
	return render_template('user.html', name=name)

if __name__ == '__main__':
	manager.run()

