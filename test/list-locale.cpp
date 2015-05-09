/*
  A script to generate the list of locales. Before running this script you need to install the locales.

  On Debian do the following:
  > apt-get install locales
  > edit /etc/locales.gen # comment out all the locales.
  > sudo locale-gen

  On other systems please search how to install the locales.
*/


#include <fstream>
#include <iostream>
#include "locale.h"
#include <string>

using namespace std;

int main () {
  ifstream ifs("/usr/share/i18n/SUPPORTED");
  while (ifs) {
    string locale, lang;
    ifs >> locale >> lang;
    if (lang=="UTF-8") {
      cout << "  (\"" << locale << "\", [" ;
      char *res;
      res = setlocale(LC_TIME, locale.c_str());
      if(!res) cerr << "FAIL" << endl;
      char buf[512];
      struct tm *tmp;
      time_t day = 86400;
      for(time_t t = 0; t < 7*day; t+=day){
        tmp = localtime(&t);
        strftime(buf, 512, "\"%a\"", tmp);
        if(t>0) cout << ", ";
        cout << buf;
      }
      cout << "])," << endl;
    }
  }
}
