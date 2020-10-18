from bs4 import BeautifulSoup
import requests
import re, os

path = "/home/zee/Desktop/CyberPk/"
check = "format.txt"
fileRead = "refined.txt"
resultfile = "siteResult.csv"
reader = open(path + check , "r")
lines= reader.readlines()


siteReader = open(path + fileRead, 'r')
lineReader = siteReader.readlines()

headers = {"User-agent":"Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/47.0.2526.80 Safari/537.36"}

http = 'http://'
#url = 'www.ecopsc.gov.pk'
#url = "alfalahinsurance.com.pk"


#print(soup.title.text)

email = 0
site = []


def checker(out):
	for l in lines:
		#print (out)
		if l.strip() in out:
			file.append(l.strip())


saver = open(path + resultfile, 'a+')
saver.write("link, LinkCount, EmailCount, FileCount, Files, LinkFormat, \n")


for r in lineReader:
	url = r.strip()
	print (url)
	try:
		if int(requests.get(http + url, headers=headers).status_code) == 200:
			html_content = requests.get(http + url, headers=headers).text
			soup = BeautifulSoup(html_content, "lxml")
			email = 0
			count = 0
			site = []
			file = []
			for link in soup.find_all("a"): 
				output = link.get("href")
				if output is not None:
					checker (output)
					if '@' in output:
						email = email + 1
					if '://' in output:
						site.append(output.split('//',1)[0])
					count = count + 1
			saver = open(path + resultfile, 'a+')
			print (str(url) + ', ' + str(count) + ', ' + str(email) + ', ' + str(len(list(set(file)))) + ', ' + str(list(set(file))) + ', ' +  str(list(set(site))) + ', ' + '\n')
			saver.write(str(url) + ', ' + str(count) + ', ' + str(email) + ', ' + str(len(list(set(file)))) + ', ' + str(list(set(file))) + ', ' +  str(list(set(site))) + ', ' + '\n')
			saver.close()
			del site
			del email
			del count
			del file
		else:
			saver = open(path + resultfile, 'a+')
			saver.write(str(url) + ', ' + str('URL Down') + ', ' + str('') + ', ' + str('') + ', ' + str('') + ', ' +  str('') + ', ' + '\n')
			saver.close()
	except:
		saver = open(path + resultfile, 'a+')
		saver.write(str(url) + ', ' + str('Access Denied') + ', ' + str('') + ', ' + str('') + ', ' + str('') + ', ' +  str('') + ', ' + '\n')
		saver.close()




'''
print ("Number of Links: " + str(count))
print ("Number of Email addresses : " + str(email))
print (list(set(file)))
print ("Number of Unique files: " + str(len(list(set(file)))))
#print ("site " + str(len(list(set(site)))))
print (str(list(set(site))))
'''


'''
for link in soup.find_all("a"): 
	#print(link.text)
	#print("Title: {}".format(link.get("title")))
	output = link.get("href")
	print (output)
	if '?' in  output:
		dynamic = 1
		if '/' in output.rsplit('?',1)[0]:
			files.append(output.rsplit('?',1)[0][output.rindex('.')+1:])
			file.append(output.rsplit('?',1)[0][output.rindex('/')+1:])
			site.append(output.split('//',1)[0])
			count = count + 1
		else:
			files.append(output.rsplit('?',1)[0][output.rindex('.')+1:])
			file.append(output.rsplit('?',1)[0])
			count = count + 1
	elif '@' in output:
		email = email + 1
	elif '://' in output:
		if output[-1] != '/':
			files.append(output.rsplit('.',1)[1])
			file.append(output.rsplit('/',1)[1])
	elif '.' in output:
		files.append(output[output.rindex('.')+1:])
		file.append(output)

print ("Number of Links: " + str(count))
print ("Number of Email addresses : " + str(email))
print (list(set(files)))
print ("Number of file types: " + str(len(list(set(files)))))
print (list(set(file)))
print ("Number of Unique files: " + str(len(list(set(file)))))
print ("site " + str(site))
print (dynamic)

'''