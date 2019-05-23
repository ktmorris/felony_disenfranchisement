### THIS CODE SCRAPES NYS PAROLE WEBSITE TO CHECK FOR
### VOTING RIGHTS RESTORATION

import selenium
from selenium import webdriver
from bs4 import BeautifulSoup
import csv

with open("H:/Public/Democracy/Voting Rights & Elections/data/felony_disenfranchisement/temp/parolees.csv", 'rb') as f:
    reader = csv.reader(f)
    your_list = list(reader)

with open("H:/Public/Democracy/Voting Rights & Elections/data/felony_disenfranchisement/temp/nys_parole_codes.csv", "wb") as t:
    writer = csv.writer(t)
    path_to_chromedriver= "H:/Public/Democracy/Voting Rights & Elections/data/misc/chromedriver.exe"

    driver = webdriver.Chrome(executable_path = path_to_chromedriver)
    i = 1
    for item in your_list:
        driver.get("http://www.doccs.ny.gov/ParoleeLookup/Lookup.aspx")

        inputElement = driver.find_element_by_id("txtDin")
        inputElement.send_keys(item)

        driver.find_element_by_id("BtnSubmit").click()
        try:
            table = driver.find_element_by_xpath("//table[@id='paroleeInformation']")

            for column in table.find_elements_by_xpath(".//tr"):
                temp = [td.text for td in column.find_elements_by_xpath(".//td")] + [i]
                writer.writerow(temp)
        except:
            pass
        i = i + 1
