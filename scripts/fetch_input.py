import sys
import requests

if len(sys.argv) < 2:
    print("Pass in the day value as a CLI argument")
    exit()

url = "https://adventofcode.com/2020/day/{}/input".format(sys.argv[1])
cookie_key = None
with open('COOKIE_KEY') as cookie:
    cookie_key = cookie.read()
    cookie.close()

r = requests.get(url, headers={'Cookie': cookie_key})
if r.status_code != 200:
    print("Input file is not yet available")
    exit()
with open('input/day{}.in'.format(sys.argv[1]), "x") as f:
    f.write(r.text)
    f.close()
