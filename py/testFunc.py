import sys
sys.path.append("D:/Library/Documents/Erlang/Elevators/_build/default/lib/erlport/priv/python2")
from erlport.erlterms import Atom
#from erlport.erlang import MessageHandler


def init(a, b):
	print "I'm a python func!"
	print "You gave me: {}, {}".format(a, b)
	return MyClass(a, b)


class MyClass:
	def __init__(self, a, b):
		self.a = a
		self.b = b

	def doYourThing(self):
		print "Doing {} my {} thing".format(self.a, self.b)