#ifndef KEYWORDCONFIG_H
#define KEYWORDCONFIG_H

#include <keywordentry.h>
#include <QFile>
#include <set>
#include "inputtab.h"

class SSTKeywordClass {

private:
    std::list<KeywordEntry*> keywords_;

    static std::map<QString,QString> defaults_;

    static std::set<QString> ignored_;

    static std::map<QString, SSTKeywordClass*> classes_;

    OpenKeywordEntry* add_string_keyword(const QString& keyname, const QString& defval, const QString& docstring);

    void init_combo(ComboKeywordEntry* entry, std::vector<QString>& typenames,
                    const QString& defval, const QString& allowed_str);

    ComboKeywordEntry* add_combo_keyword(const QString& keyname, const QString& keytype,
                                         const QString& defval, const QString& allowed_str,
                                         const QString& docstring);

    FakeKeywordEntry* add_fake_keyword(const QString& keyname, const QString& keytype,
                                           const QString& defval, const QString& allowed_str,
                                            const QString& docstring);

    YesNoKeywordEntry* add_yesno_keyword(const QString& keyname, const QString& keytype,
                                           const QString& defval, const QString& allowed_str,
                                            const QString& docstring);

    FactoryKeywordEntry* add_factory_keyword(const QString& keyname, const QString& keytype,
                                           const QString& defval, const QString& allowed_str,
                                            const QString& docstring);

    VectorKeywordEntry* add_vector_keyword(const QString& keyname, const QString& defval, const QString& docstring);

    OpenKeywordEntry* add_integer_keyword(const QString& keyname, const QString& defval, const QString& docstring);

    OpenKeywordEntry* add_double_keyword(const QString& keyname, const QString& defval, const QString& docstring);

    BooleanKeywordEntry* add_boolean_keyword(const QString& keyname, const QString& defval, const QString& docstring);

    TimestampKeywordEntry* add_timestamp_keyword(const QString& keyname, const QString& defval, const QString& docstring);

    ByteLengthKeywordEntry* add_byte_length_keyword(const QString& keyname, const QString& defval, const QString& docstring);

    BandwidthKeywordEntry* add_bandwidth_keyword(const QString& keyname, const QString& defval, const QString& docstring);

    FrequencyKeywordEntry* add_frequency_keyword(const QString& keyname, const QString& defval, const QString& docstring);

    void add_keyword(const QString& line);

    static const int keyname_index = 0;
    static const int keytype_index = 1;
    static const int clstype_index = 2;
    static const int default_index = 3;
    static const int allowed_index = 4;
    static const int docstring_index = 5;
    static const int tab_index = 6;
    static const int alias_index = 7;
    static const int extra_index = 8;
    static const int yesno_index = 9;

public:

    SSTKeywordClass();

    void add_keywords(const QString& fname);

    static void init_launcher(const QString& launcher_name);

    static void add_default_value(const QString& keyname, const QString& defval);

    static QString get_default_value(const QString& keyname);

    static void add_ignored_keyword(const QString& key);

    static bool is_ignored(const QString& key);

    void clone(InputTabLayout* tab) const;

    static SSTKeywordClass* get(const QString& type);

};


#endif // KEYWORDCONFIG_H
