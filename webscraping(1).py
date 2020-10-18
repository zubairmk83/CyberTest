
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
saver.close()


def filewriter(data):
	saver = open(path + Outfile , "a+")
	saver.write(str(data["ipAddress"]) + ", " + str(data["continentName"]) +", " + str(data["countryName"]) + ", " + str(data["stateProvCode"]) + ", " + str(data["stateProv"]) + ", " + 
			str(data["city"]) + ", " + str(data["latitude"]) + ", " + str(data["longitude"]) + ", " + str(data["isp"]) + ", " +  "\n")			
	saver.close()


def sitedb-ip(urllink,link):
	try:
		ip = socket.gethostbyname(link.strip())
		print(ip)
		#answer = myResolver.query(l.strip(), 'A')
		site= urllink + str(ip)	
		print(site)
		hdr = {'User-Agent': 'Mozilla/5.0'}
		req = Request(site,headers=hdr)
		page = urlopen(req)
		soup = BeautifulSoup(page, "lxml")
		end = soup.text.find("}",1,10000)
		output = soup.text.strip()[26:end+1]
		data = json.loads(output)
		saver = open(path + Outfile , "a+")
		saver.write(str(data["ipAddress"]) + ", " + str(data["continentName"]) +", " + str(data["countryName"]) + ", " + str(data["stateProvCode"]) + ", " + str(data["stateProv"]) + ", " + 
			str(data["city"]) + ", " + str(data["latitude"]) + ", " + str(data["longitude"]) + ", " + str(data["isp"]) + ", " +  "\n")			
		saver.close()
	except:
		#ip = socket.gethostbyname(l)
		print(str(link) + " error")


def siteipapi(urllink,link):
	try:
		ip = socket.gethostbyname(link.strip())
		print(ip)
		#answer = myResolver.query(l.strip(), 'A')
		site= urllink + str(ip)	
		print(site)
		hdr = {'User-Agent': 'Mozilla/5.0'}
		req = Request(site,headers=hdr)
		page = urlopen(req)
		soup = BeautifulSoup(page, "lxml")
		end = soup.text.find("}",1,10000)
		output = soup.text.strip()[26:end+1]
		data = json.loads(output)
		saver = open(path + Outfile , "a+")
		saver.write(str(data["ip"]) + ", " + str(data["continent_name"]) +", " + str(data["country_name"]) + ", " + str(data["region_code"]) + ", " + str(data["region_name"]) + ", " + 
			str(data["city"]) + ", " + str(data["latitude"]) + ", " + str(data["longitude"]) + ", " + str(data["isp"]) + ", " +  "\n")			
		saver.close()
	except:
		#ip = socket.gethostbyname(l)
		print(str(link) + " error")


for l in lines:
	print("-----------")
	print(l)
	#sitedb-ip("https://db-ip.com/demo/home.php?s=", l)
	siteipapi("https://ipapi.com/ip_api.php?ip=", l)




