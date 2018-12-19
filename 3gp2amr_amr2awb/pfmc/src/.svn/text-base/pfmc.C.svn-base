#include <cstdio>
#include <cstring>
#include <cstdlib>
#include <cctype>
#include <vector>
#include <string>
#include <set>
#include <fstream>
#include <interface.h>
#include <pfmcmessage.h>

#define NEWFAIL 1
#define INITFAIL 2
#define SETFAIL 3
#define WRITEFAIL 4
#define READFAIL 5
#define SENDFAIL 6

using namespace std;

PfmcApi::Interface *pfmc = NULL;

int get_name_value(const string& orig, string& name, string& value, bool upper) {
    name = "";
    value = "";
    string::const_iterator iter;
    for (iter = orig.begin(); iter != orig.end(); ++iter) {
        if (*iter == ':') {
            break;
        } else if (*iter != ' ') {
            if (upper) {
                name += *iter;
            } else {
                name += tolower(*iter);
            }
        }
    }
    for ( ++iter; iter != orig.end(); ++iter) {
        if (*iter != ' ') {
            value += *iter;
        }
    }
    return 0;
}

int get_names_values(const string& Filename, const string& key, vector<string>& names, vector<string>& values, bool withkey) {
    ifstream Fin(Filename.c_str());
    string line;
    names.clear();
    values.clear();
    while (getline(Fin, line)) {
        if (line.substr(0,key.length()) == key) {
            string name;
            string value;
            if (withkey) {
                get_name_value(line, name, value, true);
            } else {
                get_name_value(line.substr(key.length()), name, value, false);
            }
            //printf("%s\n",name.c_str());
            //printf("%s\n",value.c_str());
            names.push_back(name);
            values.push_back(value);
        }
    }
    Fin.close();
    set<string> name_filter(names.begin(), names.end());
    names.resize(name_filter.size());
    return 0;
}


PfmcApi::PfmcMessage gen_message(const string& pid,
                                 const string& datetime,
                                 const string& jobid,
                                 const string& key,
                                 bool withkey) {
    vector<string> names;
    vector<string> values;
    get_names_values(string("/dev/shm/ocarina_")+pid+".status", key, names, values, withkey);

    PfmcApi::PfmcMessage message;
    ///设置任务号
    message.setJobId(jobid.c_str());
    ///设置统计结束时间
    message.setEndTime(datetime);
    ///增加Name指标名
    for (vector<string>::iterator iter = names.begin(); iter != names.end(); ++iter) {
        //printf("%s\n",iter->c_str());
        message.addName(*iter);
    }
    if (names.size() > 0) {
        int times = values.size() / names.size();
        for (int i = 0; i < times; i++) {
            message.addNewLineForValue();
            vector<string> value_line(values.begin() + i*names.size(), values.begin() + (i+1)*names.size());
            for (vector<string>::iterator iter = value_line.begin(); iter != value_line.end(); ++iter) {
                //printf("%s\n",iter->c_str());
                message.addValue(*iter);
            }
            message.endLineForValue();
        }
    }
    return message;
}

int send_message(const PfmcApi::PfmcMessage& orig_message) {
    PfmcApi::PfmcMessage message = orig_message;
    return pfmc->sendMessage(message);
}

int main(int argc, char *argv[]) {
    if ( argc != 4) {
        return 0;
    }
    const char* pid = argv[1];
    const char* ip = argv[2];
    const char* port = argv[3];


    //生成PFMC API接口类实例
    pfmc = new PfmcApi::Interface(0, "pfmcapi.pfmc.log");
    //这就是扯，new失败直接异常
    if (!pfmc) {
        exit(NEWFAIL);
    }
    //初始化PFMC API接口类实例
    if (pfmc->init() != 0) {
        exit(INITFAIL);
    }
    if (!pfmc->setLogLevel(2)) {
        exit(SETFAIL);
    }
    //打开UDP消息跟踪
    if (!pfmc->opneUDPTrace()) {
        exit(SETFAIL);
    }

    //加载UDP消息目的IP和地址
    if (!pfmc->addDestIpAndPort(ip, atoi(port))) {
        exit(SETFAIL);
    }
    //设置小时粒度备份周期
    if (!pfmc->setHourBak()) {
        exit(SETFAIL);
    }

    char c[1];
    while(fread(c, sizeof(c), 1, stdin)) {
        if ( !(fwrite(c, sizeof(c), 1, stdout) and !fflush(stdout)) ) {
            exit(WRITEFAIL);
        }
        if ( c[0] == 'R' ) {
            char DateTime[19];
            if (1 != fread(DateTime, sizeof(DateTime), 1, stdin)) {
                exit(READFAIL);
            }
            //发送Messgae消息
            if (send_message(gen_message(pid,
                                         string(DateTime, sizeof(DateTime)),
                                         "091001",
                                         "Req",
                                         true)) < 0) {
                exit(SENDFAIL);
            }
            if (send_message(gen_message(pid,
                                         string(DateTime, sizeof(DateTime)),
                                         "091002",
                                         "Time",
                                         true)) < 0) {
                exit(SENDFAIL);
            }
            if (send_message(gen_message(pid,
                                         string(DateTime, sizeof(DateTime)),
                                         "041005",
                                         "Kpi Send",
                                         false)) < 0) {
                exit(SENDFAIL);
            }
            if (send_message(gen_message(pid,
                                         string(DateTime, sizeof(DateTime)),
                                         "041006",
                                         "Kpi Recv",
                                         false)) < 0) {
                exit(SENDFAIL);
            }
            if (send_message(gen_message(pid,
                                         string(DateTime, sizeof(DateTime)),
                                         "041007",
                                         "Rtp Send",
                                         false)) < 0) {
                exit(SENDFAIL);
            }
            if (send_message(gen_message(pid,
                                         string(DateTime, sizeof(DateTime)),
                                         "041008",
                                         "Rtp Recv",
                                         false)) < 0) {
                exit(SENDFAIL);
            }
        } else {
            //调用备份文件周期维护函数
            pfmc->active();
        }
    }
    return 0;
}
