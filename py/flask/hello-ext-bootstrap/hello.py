# python -m pip install flask-script
# python -m pip install flask-bootstrap
# python hello.py runserver --host 127.0.0.1

from flask import Flask, render_template
from flask_script import Manager
from flask_bootstrap import Bootstrap

app = Flask(__name__)
manager = Manager(app)
bootstrap = Bootstrap(app)

@app.route("/")
def index():
	return render_template('index.html')

@app.route("/user/<name>")
def user(name):
	return render_template('user.html', name=name)

if __name__ == '__main__':
	manager.run()

