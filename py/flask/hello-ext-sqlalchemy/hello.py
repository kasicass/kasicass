# python -m pip install flask-script
# python -m pip install flask-bootstrap
# python -m pip install flask-wtf
# python -m pip install flask-sqlalchemy
# 
# python hello.py shell
# >>> from hello import db
# >>> db.create_all()           # re-create tables, db.drop_all(), then db.create_all()
#
# python hello.py runserver --host 127.0.0.1

import os
from flask import Flask, render_template, session, redirect, url_for, flash
from flask_script import Manager
from flask_sqlalchemy import SQLAlchemy
from flask_bootstrap import Bootstrap
from flask_wtf import FlaskForm
from wtforms import StringField, SubmitField
from wtforms.validators import Required

basedir = os.path.abspath(os.path.dirname(__file__))

app = Flask(__name__)
app.config['SQLALCHEMY_DATABASE_URI'] = 'sqlite:///' + os.path.join(basedir, 'data.sqlite')
app.config['SQLALCHEMY_COMMIT_ON_TEARDOWN']	= True
app.config['SQLALCHEMY_TRACK_MODIFICATIONS'] = False
app.config['SECRET_KEY'] = 'abc'  # for wtf

db = SQLAlchemy(app)
manager = Manager(app)
bootstrap = Bootstrap(app)

class Role(db.Model):
	__tablename__ = 'roles'
	id		= db.Column(db.Integer, primary_key=True)
	name	= db.Column(db.String(64), unique=True)
	users = db.relationship('User', backref='role')

	def __repr__(self):
		return '<Role %r>' % self.name

class User(db.Model):
	__tablename__ = 'users'
	id = db.Column(db.Integer, primary_key=True)
	username = db.Column(db.String(64), unique=True, index=True)
	role_id = db.Column(db.Integer, db.ForeignKey('roles.id'))

	def __repr__(self):
		return '<User %r>' % self.username


class NameForm(FlaskForm):
	name = StringField('What is your name?', validators=[Required()])
	submit = SubmitField('Submit')

@app.route("/", methods=['GET', 'POST'])
def index():
	form = NameForm()
	if form.validate_on_submit():
		user = User.query.filter_by(username=form.name.data).first()
		if user is None:
			user = User(username = form.name.data)
			db.session.add(user)
			session['known'] = False
		else:
			session['known'] = True
		session['name'] = form.name.data
		form.name.data = ''
		return redirect(url_for('index'))

	return render_template('index.html',
		form=form, name=session.get('name'),
		known=session.get('known', False))

@app.route("/user/<name>")
def user(name):
	return render_template('user.html', name=name)

@app.errorhandler(404)
def page_not_found(e):
	return render_template('404.html'), 404


if __name__ == '__main__':
	manager.run()

