
a = 2 - (3 + 4)
b = 44
b = a + 1

idk = "heyo"

def stuff(a, b = 4):
	global idk

	if True:
		idk = "wat"

	if False:
		idk = "nope"
	
	def double(v):
		return v + v

	return double(a) + "whoa!!! "

message = stuff("...") + "hi " + "there"
return message + idk
