
import os, socket
import json
from bs4 import BeautifulSoup
from urllib.request import Request, urlopen
import dns.resolver
 
myResolver = dns.resolver.Resolver(configure=False)
myResolver.nameservers = ['8.8.8.8', '8.8.4.4']


path = "/home/zee/Desktop/CyberPk/"
check = "site.txt"
Outfile = "wescrap.csv"
reader = open(path + check , "r")
lines= reader.readlines()


saver = open(path + Outfile , "a+")
saver.write("\n" + "IPAddress, " + "Continent, "  + "Country, " + "StateProvCode, " + "StateProv, " + "District, " + "City, " + "Latitude, " + "Longitude, " + "ISP, " + "Organization, " + "\n")



for l in lines:
	print("-----------")
	print(l)
	try:
		answer = myResolver.query(l.strip(), 'A')
		for b in answer:
			site= "https://db-ip.com/demo/home.php?s=" + str(b)	
			#ip = socket.gethostbyname(answer[0])
			hdr = {'User-Agent': 'Mozilla/5.0'}
			req = Request(site,headers=hdr)
			page = urlopen(req)
			soup = BeautifulSoup(page, "lxml")
			end = soup.text.find("}",1,10000)
			output = soup.text.strip()[26:end+1]
			data = json.loads(output)
			saver.write(str(data["ipAddress"]) + ", " + str(data["continentName"]) +", " + str(data["countryName"]) + ", " + str(data["stateProvCode"]) + ", " + str(data["stateProv"]) + ", " + 
				str(data["district"]) + ", " + str(data["city"]) + ", " + str(data["latitude"]) + ", " + str(data["longitude"]) + ", " + str(data["isp"]) + ", " + str(data["organization"]) + ", "+ "\n")			
			#print(data)
	except:
		#ip = socket.gethostbyname(l)
		print(l + " error")


saver.close()

