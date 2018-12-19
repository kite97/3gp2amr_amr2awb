#!/usr/bin/python
#Filename:create_config.py

"""create_config.py:Automatically create common message config file"""

import sys
import os

config_xml_header = '''<?xml version="1.1" encoding="UTF-8" ?>'''
config_root_s     = '''<configuration>'''
config_program_s  = '''    <programs>'''
config_fe         = '''        <fe id="%d" name="%s" forward="%d" executable="%s" timeout="%d" />'''
config_program_e  = '''    </programs>'''
config_process_s  = '''    <processes>'''
config_domain_s   = '''        <domain id="%d" name="%s" host="%s">'''
config_subnet     = '''            <subnet name="%s" fe="%s" startinstance="%d" number="%d" initialnumber="%d" />'''
config_domain_e   = '''        </domain>'''
config_process_e  = '''    </processes>'''
config_link_s     = '''    <links>'''
config_linkset    = '''        <linkset domain1="%s" subnet1="%s" domain2="%s" subnet2="%s" startport="%d" />'''
config_link_e     = '''    </links>'''
config_root_e     = '''</configuration>'''

fe_list = ({"name":"ininit",    "id":1,   "forward":0,  "executable":"ininit",    "timeout":15  },
           {"name":"msgr",      "id":11,  "forward":1,  "executable":"msgr",      "timeout":15  },
           {"name":"rcs",       "id":21,  "forward":0,  "executable":"rcpp",      "timeout":15  },
           {"name":"rps",       "id":27,  "forward":0,  "executable":"rppp",      "timeout":300 },
           {"name":"rms",       "id":31,  "forward":0,  "executable":"rmpp",      "timeout":15  },
           {"name":"ringmgr",   "id":41,  "forward":0,  "executable":"ringmgr",   "timeout":15  },
           {"name":"inaccessd", "id":63,  "forward":0,  "executable":"inaccessd", "timeout":15  })

########################################################################
#                          Instructions                                #
########################################################################
print "\nCommon Message Component config file generator for NCSP-MS. Version 0.9.0"
print "\n> Ctrl+Backspace : delete previous charactor"
print "> Ctrl+C/Ctrl+D  : terminate program\n"

########################################################################
#                  Start to Read config information                    #
########################################################################

# the function will get user's non-empty input if default_str is not ""
def get_input_str(tip_str,default_str="") :
    str_fin = ""
    while str_fin=="" :
        try :
            str_fin = raw_input(tip_str)
            if str_fin=="" :
                str_fin = default_str
        except EOFError :
            print
            sys.exit()
        except KeyboardInterrupt :
            print
            sys.exit()
        except :
            print '\nError when accepting input'
            sys.exit()

    return str_fin

def get_input_int(tip_str,default_int=-9999) :
    int_fin = -9999
    while int_fin==-9999 :
        str_inp = get_input_str(tip_str, "%d"%(default_int,))
        try :
            int_fin = int(str_inp)
        except :
            print "A number is needed here,please input again"
            continue

    return int_fin

def get_scope_int(tip_str,min,max,default_int=-9999) :
    int_fin = -9999
    while int_fin==-9999 :
        str_inp = get_input_str(tip_str, "%d"%(default_int,))
        try :
            int_fin = int(str_inp)
            if int_fin < min or int_fin > max:
                print "The number is out of scope[%d-%d],please input again"%(min,max)
                int_fin=-9999
                continue
        except :
            print "A number is needed here,please input again"
            continue

    return int_fin

# Input RCS Domain Information
rcs_domain = 0
while rcs_domain<1 or rcs_domain>100 :
    rcs_domain = get_input_int('How many RCS domains do you want:[1-100] (1)', 1)

rcs_domain_list = []

rcs_index = 1
while rcs_index<=rcs_domain:
    domain_attr = {}
    domain_attr['id']   = get_scope_int("RCS %d - Domain ID scope:[1-100]:"%(rcs_index,),1,100)
    domain_attr['name'] = "rcs%d.domain"%(domain_attr['id'],)
    domain_attr['host'] = get_input_str("RCS %d - IP Addr  :"%(rcs_index,))
    result = get_input_str("RCS %d:DomainID=<%d>, IP=<%s>, is it rignt?[y/n] (y)"%(rcs_index,domain_attr['id'], domain_attr['host']), "y")

    if result=="y" :
        rcs_index += 1
        rcs_domain_list.append(domain_attr)

# Input RPS Domain Information
rps_domain = 0
print  # print an empty line
while rps_domain<1 or rps_domain>100 :
    rps_domain = get_input_int('How many RPS domains do you want:[1-100] (1)', 1)

rps_domain_list = []

rps_index = 1
while rps_index<=rps_domain:
    domain_attr = {}
    domain_attr['id']   = get_scope_int("RPS %d - Domain ID scope:[101-200]:"%(rps_index,),101,200)
    domain_attr['name'] = "rps%d.domain"%(domain_attr['id'],)
    domain_attr['host'] = get_input_str("RPS %d - IP Addr  :"%(rps_index,))
    domain_attr['boardtype'] = get_input_int("RPS %d:Please choose board type:[1]HMP,[2]IPS (1)"%(rps_index,), 1)
    if domain_attr['boardtype']==1 :
        boardname = "HMP"
    else :
        boardname = "IPS"
    result = get_input_str("RPS %d:DomainID=<%d>, IP=<%s>, board name is <%s>, is it rignt?(y/n) (y)"%(rps_index,domain_attr['id'], domain_attr['host'],boardname), "y")

    if result=="y" :
        rps_index += 1
        rps_domain_list.append(domain_attr)

# Input RMS Domain Information
rms_domain = -1
print  # print an empty line
while rms_domain<0 or rms_domain>2 :
    rms_domain = get_input_int('How many RMS domains do you want:[0,1,2] (0)', 0)

rms_domain_list = []

rms_index = 1
while rms_index<=rms_domain :
    domain_attr = {}
    domain_attr['id']   = get_scope_int("RMS %d - Domain ID[201-254]:"%(rms_index,),201,254)
    domain_attr['name'] = "rms%d.domain"%(domain_attr['id'],)
    domain_attr['host'] = get_input_str("RMS %d - IP Addr  :"%(rms_index,))
    result = get_input_str("RMS %d:DomainID=<%d>, IP=<%s>, is it rignt?(y/n) (y)"%(rms_index,domain_attr['id'], domain_attr['host']), "y")

    if result=="y" :
        rms_index += 1
        rms_domain_list.append(domain_attr)

# Input port base
print "\nEach domain need 2 ports to listen to other clients.Please input a base port number,and all ports to be used will increase from it."
port_base = 0
while port_base<1024 or port_base>65535 :
    port_base = get_input_int("Input a base port[1024~65535]:(34510)", 34510)

# Input filename
print  # print an empty line
confirmed = True
while confirmed :
    filename = get_input_str("Output filename:(config.comm)", "config.comm")
    if os.path.exists(filename) :
        result = get_input_str("File <%s> is exists, do you want to override it?(y/n) (n)"%(filename,), "n")
        if result=="y" :
            confirmed = False
    else :
        confirmed = False

########################################################################
#                  Start to Create config text                         #
########################################################################
final_config = ""

final_config += config_xml_header + "\n"  # XML Header
final_config += config_root_s + "\n"      # XML Root Element Start

#####
## START program settings

final_config += config_program_s + "\n"

# config each fe
for fe_inf in fe_list :
    final_config += config_fe%(fe_inf["id"],fe_inf["name"],fe_inf["forward"],fe_inf["executable"],fe_inf["timeout"]) + "\n"

final_config += config_program_e + "\n\n"

## END program settings
#####


#####
## START process settings

final_config += config_process_s + "\n"

# config rcs domain
for rcs_domain_inf in rcs_domain_list :
    final_config += config_domain_s%(rcs_domain_inf["id"],rcs_domain_inf["name"],rcs_domain_inf["host"]) + "\n"
    # config domain subnet
    final_config += config_subnet%("ininit",    "ininit",    1,  1, 1) + "\n"
    final_config += config_subnet%("msgr",      "msgr",      4,  1, 1) + "\n"
    final_config += config_subnet%("rcs",       "rcs",       8,  1, 1) + "\n"
    #final_config += config_subnet%("inaccessd", "inaccessd", 16, 1, 1) + "\n"
    final_config += config_domain_e + "\n\n"

# config rps domain
for rps_domain_inf in rps_domain_list :
    final_config += config_domain_s%(rps_domain_inf["id"],rps_domain_inf["name"],rps_domain_inf["host"]) + "\n"
    # config domain subnet
    final_config += config_subnet%("ininit",    "ininit",    1,  1, 1) + "\n"
    final_config += config_subnet%("msgr",      "msgr",      4,  1, 1) + "\n"
    final_config += config_subnet%("rps",       "rps",       8,  1, 1) + "\n"
    if rps_domain_inf['boardtype']==1 :
        final_config += config_subnet%("ringmgr",   "ringmgr",   16, 1, 1) + "\n"
    #final_config += config_subnet%("inaccessd", "inaccessd", 32, 1, 1) + "\n"
    final_config += config_domain_e + "\n\n"

# config rms domain
for rms_domain_inf in rms_domain_list :
    final_config += config_domain_s%(rms_domain_inf["id"],rms_domain_inf["name"],rms_domain_inf["host"]) + "\n"
    # config domain subnet
    final_config += config_subnet%("ininit",    "ininit",    1,  1, 1) + "\n"
    final_config += config_subnet%("msgr",      "msgr",      4,  1, 1) + "\n"
    final_config += config_subnet%("rms",       "rms",       8,  1, 1) + "\n"
    #final_config += config_subnet%("inaccessd", "inaccessd", 16, 1, 1) + "\n"
    final_config += config_domain_e + "\n"

final_config += config_process_e + "\n"

## END process settings
#####


#####
## START link settings
final_config += config_link_s + "\n"

port_inc  = 0

def get_next_port() :
    global port_base
    global port_inc

    ret_port = port_base + port_inc
    port_inc += 1

    return ret_port

# config rcs links
for rcs_domain_inf in rcs_domain_list :
    rcs_ininit_port = get_next_port()
    rcs_msgr_port   = get_next_port()
    final_config += "\n"
    final_config += config_linkset%(rcs_domain_inf["name"], "ininit", rcs_domain_inf["name"], "msgr",      rcs_ininit_port) + "\n"
    final_config += config_linkset%(rcs_domain_inf["name"], "ininit", rcs_domain_inf["name"], "rcs",       rcs_ininit_port) + "\n"
    #final_config += config_linkset%(rcs_domain_inf["name"], "ininit", rcs_domain_inf["name"], "inaccessd", rcs_ininit_port) + "\n"
    final_config += config_linkset%(rcs_domain_inf["name"], "msgr",   rcs_domain_inf["name"], "rcs",       rcs_msgr_port) + "\n"
    #final_config += config_linkset%(rcs_domain_inf["name"], "msgr",   rcs_domain_inf["name"], "inaccessd", rcs_msgr_port) + "\n"
    # add rps links
    for rps_domain_inf in rps_domain_list :
        final_config += config_linkset%(rcs_domain_inf["name"], "msgr",   rps_domain_inf["name"], "msgr",      rcs_msgr_port) + "\n"

# config rps links
for rps_domain_inf in rps_domain_list :
    rps_ininit_port = get_next_port()
    rps_msgr_port   = get_next_port()
    final_config += "\n"
    final_config += config_linkset%(rps_domain_inf["name"], "ininit", rps_domain_inf["name"], "msgr",      rps_ininit_port) + "\n"
    final_config += config_linkset%(rps_domain_inf["name"], "ininit", rps_domain_inf["name"], "rps",       rps_ininit_port) + "\n"
    if rps_domain_inf['boardtype']==1 :
        final_config += config_linkset%(rps_domain_inf["name"], "ininit", rps_domain_inf["name"], "ringmgr",   rps_ininit_port) + "\n"
    #final_config += config_linkset%(rps_domain_inf["name"], "ininit", rps_domain_inf["name"], "inaccessd", rps_ininit_port) + "\n"
    final_config += config_linkset%(rps_domain_inf["name"], "msgr",   rps_domain_inf["name"], "rps",       rps_msgr_port) + "\n"
    if rps_domain_inf['boardtype']==1 :
        final_config += config_linkset%(rps_domain_inf["name"], "msgr",   rps_domain_inf["name"], "ringmgr",   rps_msgr_port) + "\n"
    #final_config += config_linkset%(rps_domain_inf["name"], "msgr",   rps_domain_inf["name"], "inaccessd", rps_msgr_port) + "\n"

# config rms links
for rms_domain_inf in rms_domain_list :
    rms_ininit_port = get_next_port()
    rms_msgr_port   = get_next_port()
    final_config += "\n"
    final_config += config_linkset%(rms_domain_inf["name"], "ininit", rms_domain_inf["name"], "msgr",      rms_ininit_port) + "\n"
    final_config += config_linkset%(rms_domain_inf["name"], "ininit", rms_domain_inf["name"], "rms",       rms_ininit_port) + "\n"
    #final_config += config_linkset%(rms_domain_inf["name"], "ininit", rms_domain_inf["name"], "inaccessd", rms_ininit_port) + "\n"
    final_config += config_linkset%(rms_domain_inf["name"], "msgr",   rms_domain_inf["name"], "rms",       rms_msgr_port) + "\n"
    #final_config += config_linkset%(rms_domain_inf["name"], "msgr",   rms_domain_inf["name"], "inaccessd", rms_msgr_port) + "\n"
    # add rcs links
    for rcs_domain_inf in rcs_domain_list :
        final_config += config_linkset%(rms_domain_inf["name"], "msgr",   rcs_domain_inf["name"], "msgr",      rms_msgr_port) + "\n"
    # add rps links
    for rps_domain_inf in rps_domain_list :
        final_config += config_linkset%(rms_domain_inf["name"], "msgr",   rps_domain_inf["name"], "msgr",      rms_msgr_port) + "\n"
    # add other RMSs
    if rms_domain_inf!=rms_domain_list[0] :
        final_config += config_linkset%(rms_domain_inf["name"], "msgr",   rms_domain_list[0]["name"], "msgr",  rms_msgr_port) + "\n"

final_config += config_link_e + "\n"

## END link settings
#####


final_config += config_root_e + "\n" # XML Root Element Stop

# Output config text to stdout
print "\nConfig text generated:\n\n",final_config

# Output config text to file
try :
    config_file = file(filename,"w") # Open file to write
    config_file.write(final_config)
except :
    print "\nwrite to file %s failed!"%filename
else :
    print "\nwrite to file %s OK!"%filename

config_file.close()
