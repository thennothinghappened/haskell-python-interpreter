
a = 2 - (3 + 4)
b = 44
b = a + 1

idk = "heyo"

def stuff(a, b = 4):
	global idk
	idk = "wat"
	
	def double(v):
		return v + v

	return double(a) + "whoa!!! "

message = stuff("...") + "hi " + "there"
return True
