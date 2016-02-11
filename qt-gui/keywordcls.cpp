#include "keywordentry.h"
#include "keywordcls.h"
#include "util.h"
#include "inputtab.h"
#include <QtCore/QTextStream>

std::map<QString,SSTKeywordClass*> SSTKeywordClass::classes_;
std::set<QString> SSTKeywordClass::ignored_;
std::map<QString,QString> SSTKeywordClass::defaults_;

SSTKeywordClass::SSTKeywordClass()
{
}

void
SSTKeywordClass::add_keywords(const QString &fname)
{
    QFile file(fname);
    if (!file.open(QIODevice::ReadOnly | QIODevice::Text)){
        QString errorstr = "Invalid input file " + fname;
        exit_dialog(errorstr);
    }
    QTextStream in(&file);
    while (!in.atEnd()){
        QString line = in.readLine();
        add_keyword(line);
    }

    file.close();
}

OpenKeywordEntry*
SSTKeywordClass::add_string_keyword(const QString& keyname, const QString& defval, const QString& docstring)
{
    OpenKeywordEntry* entry = new OpenKeywordEntry(keyname, "string", defval, docstring);
    keywords_.push_back(entry);
    return entry;
}

void
SSTKeywordClass::init_combo(ComboKeywordEntry *entry,
                            std::vector<QString> &typenames,
                            const QString &defval,
                            const QString &allowed_str)
{

    QStringList allowed_vals = allowed_str.split(second_delim);
    for (int i=0; i < allowed_vals.size(); ++i){
        QStringList next_entry = allowed_vals[i].split(third_delim);
        if (next_entry.size() < ComboKeywordEntry::num_entries){
            QString error_msg = "Invalid string specifying allowed value: " + allowed_vals[i];
            exit_dialog(error_msg);
        }
        entry->add_option_with_tooltip(next_entry[ComboKeywordEntry::value_index], next_entry[ComboKeywordEntry::docstring_index]);
        if (next_entry[ComboKeywordEntry::typename_index].size()){ // the name actually maps to a type, not just a string value
            typenames.push_back(next_entry[ComboKeywordEntry::typename_index]);
        }
    }

    if (defval.size()){
        entry->set_value(defval);
    }

    keywords_.push_back(entry);
}

FakeKeywordEntry*
SSTKeywordClass::add_fake_keyword(const QString& keyname, const QString& keytype,
                                     const QString& defval, const QString& allowed_str,
                                     const QString& docstring)
{
    FakeKeywordEntry* entry = new FakeKeywordEntry(keyname, keytype, defval, docstring);
    std::vector<QString> typenames;
    init_combo(entry, typenames, defval, allowed_str);
    for (unsigned int i=0; i < typenames.size(); ++i){
        entry->add_typename(typenames[i]);
    }
    return entry;
}

YesNoKeywordEntry*
SSTKeywordClass::add_yesno_keyword(const QString& keyname, const QString& keytype,
                                     const QString& defval, const QString& allowed_str,
                                     const QString& docstring)
{
    YesNoKeywordEntry* entry = new YesNoKeywordEntry(keyname, keytype, defval, docstring);
    std::vector<QString> typenames;
    init_combo(entry, typenames, defval, allowed_str);
    for (unsigned int i=0; i < typenames.size(); ++i){
        entry->add_typename(typenames[i]);
    }
    return entry;
}

FactoryKeywordEntry*
SSTKeywordClass::add_factory_keyword(const QString& keyname, const QString& keytype,
                                     const QString& defval, const QString& allowed_str,
                                     const QString& docstring)
{
    FactoryKeywordEntry* entry = new FactoryKeywordEntry(keyname, keytype, defval, docstring);
    std::vector<QString> typenames;
    init_combo(entry, typenames, defval, allowed_str);
    for (unsigned int i=0; i < typenames.size(); ++i){
        entry->add_typename(typenames[i]);
    }
    return entry;
}

ComboKeywordEntry*
SSTKeywordClass::add_combo_keyword(const QString& keyname, const QString& keytype,
                                   const QString& defval, const QString& allowed_str,
                                   const QString& docstring)
{
    ComboKeywordEntry* entry = new ComboKeywordEntry(keyname, keytype, defval, docstring);
    std::vector<QString> not_used;
    init_combo(entry, not_used, defval, allowed_str);
    return entry;
}

void
SSTKeywordClass::add_ignored_keyword(const QString &key)
{
    ignored_.insert(key);
}

bool
SSTKeywordClass::is_ignored(const QString &key)
{
    return ignored_.find(key) != ignored_.end();
}

void
SSTKeywordClass::add_default_value(const QString &keyname, const QString &defval)
{
    defaults_[keyname] = defval;
}

QString
SSTKeywordClass::get_default_value(const QString &keyname)
{
    std::map<QString,QString>::const_iterator it = defaults_.find(keyname);
    if (it == defaults_.end()){
        return "";
    }
    return it->second;
}

static QString allowed_apps(
        "user_app_cxx/sstmac::sw::user_app_cxx/Launches externally compiled C++ application"
        ";user_app_c/sstmac::sw::user_app_c/Launches externally compiled C application"
        ";user_app_f90/sstmac::sw::user_app_c/Launches externally compiled F90 application"
        ";parsedumpi/sstmac::sw::parsedumpi/Application that replays DUMPI trace"
        ";mpi_test_all/sstmac::sw::mpi_testall/Run a simple MPI application that tests most MPI functions"
        ";gtc/gtc::gtc_main/Gyro-Toroidal kinetic Code primarily used for simulating magnetically confined plasmas"
);

void
SSTKeywordClass::init_launcher(const QString& keytype)
{
    SSTKeywordClass* launcher_cls = SSTKeywordClass::get(keytype);
    launcher_cls->add_factory_keyword("launch_app1", "sstmac::sw::app",
                                      "user_app_cxx", allowed_apps,
                                      "The application that will be launched by the simulation");
    launcher_cls->add_string_keyword("launch_app1_argv", "", "The argv parameters that would be passed into the application on the command line");
    launcher_cls->add_string_keyword("launch_app1_cmd", "aprun -n 2 -N 1",
                                     "Specify the launch command that would be used on a real machine to run the application.\n"
                                     "The simplest is aprun, which for MPI specifies n=number of ranks and N=number of ranks per node");\
    //launcher_cls->add_timestamp_keyword("launch_app1_start", "0.0")
}


void
SSTKeywordClass::add_keyword(const QString &line)
{
    QStringList fields = line.split(first_delim);
    QString keyname = fields.at(keyname_index);
    QString keytype = fields.at(keytype_index);
    QString clstype = fields.at(clstype_index);
    QString defval = fields.at(default_index);
    QString allowed_str = fields.at(allowed_index);
    QString docstring = fields.at(docstring_index);
    QString extra_str = fields.at(extra_index);
    QString yesno_str = fields.at(yesno_index);
    if (docstring.size() == 0){
        docstring = "No documentation";
    }

    if (is_ignored(keyname))
        return;

    if (defval.size() == 0){
        defval = get_default_value(keyname);
    }

    KeywordEntry* entry = 0;
    if (keytype == "boolean"){
        entry = add_boolean_keyword(keyname, defval, docstring);
    }
    else if (keytype == "double"){
        entry = add_double_keyword(keyname, defval, docstring);
    }
    else if (keytype == "bandwidth"){
        entry = add_bandwidth_keyword(keyname, defval, docstring);
    }
    else if (keytype == "length"){
        entry = add_byte_length_keyword(keyname, defval, docstring);
    }
    else if (keytype == "frequency"){
        entry = add_frequency_keyword(keyname, defval, docstring);
    }
    else if (keytype == "timestamp"){
        entry = add_timestamp_keyword(keyname, defval, docstring);
    }
    else if (keytype == "vector"){
        entry = add_vector_keyword(keyname, defval, docstring);
    }
    else if (keytype == "integer"){
        entry = add_integer_keyword(keyname,defval, docstring);
    }
    else if (keytype == "string"){
        if (clstype == "open"){
            entry = add_string_keyword(keyname, defval, docstring);
        }
        else if (clstype == "combo"){

            entry = add_combo_keyword(keyname, keytype, defval, allowed_str, docstring);
        }
    }
    else if (clstype == "fake"){
        entry = add_fake_keyword(keyname, keytype, defval, allowed_str, docstring);
    }
    else if (clstype == "yesno"){
        entry = add_yesno_keyword(keyname, keytype, defval, allowed_str, docstring);
    }
    else if (clstype == "factory"){
        entry = add_factory_keyword(keyname, keytype, defval, allowed_str, docstring);
    }
    else {
        QString errorstr = "Invalid keyword type " + keytype + " for keyword " + keyname;
        exit_dialog(errorstr);
    }

    QString alias = fields.at(alias_index);
    if (alias.size() > 0){
        InputTab::add_aliased_type(keytype, alias);
    }

    if (extra_str == "true"){
        entry->set_as_extra();
    }
}

VectorKeywordEntry*
SSTKeywordClass::add_vector_keyword(const QString &keyname, const QString& defval, const QString& docstring)
{
    VectorKeywordEntry* entry = new VectorKeywordEntry(keyname, defval, docstring);
    keywords_.push_back(entry);
    return entry;
}

OpenKeywordEntry*
SSTKeywordClass::add_integer_keyword(const QString& keyname, const QString& defval, const QString& docstring)
{
    OpenKeywordEntry* entry = new OpenKeywordEntry(keyname, "integer", defval, docstring);
    keywords_.push_back(entry);
    return entry;
}

OpenKeywordEntry*
SSTKeywordClass::add_double_keyword(const QString& keyname, const QString& defval, const QString& docstring)
{
    OpenKeywordEntry* entry = new OpenKeywordEntry(keyname, "float", defval, docstring);
    keywords_.push_back(entry);
    return entry;
}

BooleanKeywordEntry*
SSTKeywordClass::add_boolean_keyword(const QString& keyname, const QString& defval, const QString& docstring)
{
    BooleanKeywordEntry* entry = new BooleanKeywordEntry(keyname, defval, docstring);
    keywords_.push_back(entry);
    return entry;
}

TimestampKeywordEntry*
SSTKeywordClass::add_timestamp_keyword(const QString& keyname, const QString& defval, const QString& docstring)
{
    TimestampKeywordEntry* entry = new TimestampKeywordEntry(keyname, defval, docstring);
    keywords_.push_back(entry);
    return entry;
}

FrequencyKeywordEntry*
SSTKeywordClass::add_frequency_keyword(const QString& keyname, const QString& defval, const QString& docstring)
{
    FrequencyKeywordEntry* entry = new FrequencyKeywordEntry(keyname, defval, docstring);
    keywords_.push_back(entry);
    return entry;
}

ByteLengthKeywordEntry*
SSTKeywordClass::add_byte_length_keyword(const QString& keyname, const QString& defval, const QString& docstring)
{
    ByteLengthKeywordEntry* entry = new ByteLengthKeywordEntry(keyname, defval, docstring);
    keywords_.push_back(entry);
    return entry;
}

BandwidthKeywordEntry*
SSTKeywordClass::add_bandwidth_keyword(const QString& keyname, const QString& defval, const QString& docstring)
{
    BandwidthKeywordEntry* entry = new BandwidthKeywordEntry(keyname, defval, docstring);
    keywords_.push_back(entry);
    return entry;
}

SSTKeywordClass*
SSTKeywordClass::get(const QString &type)
{
    SSTKeywordClass*& cls = classes_[type];
    if (!cls){
        cls = new SSTKeywordClass;
    }
    return cls;
}

void
SSTKeywordClass::clone(InputTabLayout *tab) const
{
    std::list<KeywordEntry*>::const_iterator it = keywords_.begin(), end = keywords_.end();
    for ( ; it != end; ++it){
        KeywordEntry* entry = *it;
        KeywordEntry* clone = entry->clone();
        tab->add_entry(clone);
    }
}
