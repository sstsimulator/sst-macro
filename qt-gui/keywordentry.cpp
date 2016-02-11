#include <keywordentry.h>
#include "keywordcls.h"
#include "inputtab.h"
#include "util.h"
#include "fonts.h"

std::map<QString,KeywordEntry*> KeywordEntry::active_entries_;
std::map<QString,QString> KeywordEntry::mandatory_values_;
std::map<QString,QString> ByteLengthKeywordEntry::clean_units_map_;
std::map<QString,QString> BandwidthKeywordEntry::clean_units_map_;
std::map<QString,QString> TimestampKeywordEntry::clean_units_map_;
std::map<QString,QString> FrequencyKeywordEntry::clean_units_map_;
std::map<QString,std::map<QString,QString> > KeywordEntry::synonyms_;
QTextEdit* KeywordEntry::external_edit_;



LoggerParam::LoggerParam(const QString& param_display,
                         const QString& param_name,
                         const QString& docstring,
                         QLayout *layout)
    : cbox_(new QCheckBox(param_display)), param_name_(param_name)
{
    layout->addWidget(cbox_);
    layout->setAlignment(cbox_, Qt::AlignHCenter);
    cbox_->setToolTip(docstring);
    cbox_->setMaximumHeight(KeywordEntry::widget_height);
    cbox_->setMinimumHeight(KeywordEntry::widget_height);
}

KeywordEntry::KeywordEntry(const QString &keyname, const QString& keytype,
                           const QString& defval, const QString& docstring) :
    my_label_(0), my_widget_(0), docstring_(docstring),
    keyname_(keyname), keytype_(keytype),
    defval_(defval),
    is_extra_param_(false),
    include_box_(0)
{
    docstring_ = docstring_.replace(QString("ENDL "), QString("\n"));
    docstring_ = docstring_.replace(QString("ENDL"), QString("\n"));
    keytype_string_ = "[" + keytype + "]";
    keyname_string_ = keyname.leftJustified(keyname_length_);
    include_box_ = new QCheckBox("Include?");
}

void
KeywordEntry::add_mandatory_value(const QString &keyname, const QString &keyval)
{
    mandatory_values_[keyname] = keyval;
}

void
KeywordEntry::clear_mandatory_values()
{
    mandatory_values_.clear();
}

QString
KeywordEntry::get_synonym(const QString &keyname, const QString &value)
{
    return synonyms_[keyname][value];
}

void
KeywordEntry::init_synonyms()
{
    synonyms_["topology_name"]["hdtorus"] = "torus";
}

void
KeywordEntry::init()
{
    if (!my_widget_){
        QHBoxLayout* my_layout = new QHBoxLayout;
        my_layout->setMargin(0);


        my_widget_ = new QWidget;
        my_widget_->setLayout(my_layout);
        my_widget_->setToolTip(docstring_);

        QWidget* widget = get_value_widget();
        widget->setMaximumWidth(widget_width);
        widget->setMinimumWidth(widget_width);
        widget->setMaximumHeight(widget_height);
        widget->setMinimumHeight(widget_height);

        my_widget_->layout()->addWidget(get_value_widget());

        QLabel* label = new QLabel(keytype_string_);


        label->setMinimumWidth(label_width);
        label->setMaximumWidth(label_width);
        my_widget_->layout()->addWidget(label);

        my_widget_->layout()->addWidget(include_box_);
        include_box_->setVisible(is_extra_param_);
        include_box_->setChecked(!is_extra_param_);

        my_layout->setAlignment(widget, Qt::AlignVCenter);

        my_label_ = new QLabel(keyname_string_);
        my_label_->setAlignment(Qt::AlignVCenter);
        my_label_->setAlignment(Qt::AlignLeft);
        my_label_->setToolTip(docstring_);
        my_label_->setFixedWidth(name_width);

        QFont font(Fonts::EntryName, Fonts::EntrySize);
        label->setFont(font);
        my_label_->setFont(font);
    }
}

void
KeywordEntry::activate(QTabWidget* widget)
{
    active_entries_[keyname_] = this;
    std::map<QString,QString>::iterator it = mandatory_values_.find(keyname_);
    if (it != mandatory_values_.end()){
        set_value(it->second);
        mandatory_values_.erase(it);
    }
}

void
KeywordEntry::deactivate()
{
    active_entries_.erase(keyname_);
}

void
KeywordEntry::clone_into(KeywordEntry *entry) const
{
    entry->is_extra_param_ = is_extra_param_;
}

ComboKeywordEntry::ComboKeywordEntry(const QString &keyname, const QString& keytype,
                                     const QString& defval, const QString& docstring)
    : KeywordEntry(keyname, keytype, defval, docstring)
{
    cbox_ = new QComboBox;

    QFont font(Fonts::EntryName, Fonts::EntrySize);
    cbox_->setFont(font);
}

KeywordEntry*
ComboKeywordEntry::clone() const
{
    ComboKeywordEntry* entry = new ComboKeywordEntry(keyname_, keytype_,
                                                     defval_, docstring_);
    for (int i=0; i < cbox_->count(); ++i){
        QString item_text = cbox_->itemText(i);
        entry->add_option_with_tooltip(item_text, tooltips_[i]);
    }
    entry->set_value(cbox_->currentText());
    KeywordEntry::clone_into(entry);
    return entry;
}

void
ComboKeywordEntry::set_value(const QString &val)
{
    int idx = cbox_->findText(val);
    if (idx == -1){
        //try to get a synonym
        QString synonym = get_synonym(keyname_, val);
        idx = cbox_->findText(synonym);
    }

    if (idx == -1){
        QString errorstr = "Invalid value \"" + val
                            + "\" for combo box "
                            "keyname=" + keyname_ + "\n"
                            "keytype=" + keytype_ + "\n";
        QString allowedstr; allowedstr.sprintf("The %d allowed values are:\n", cbox_->count());
        for (int i=0; i < cbox_->count(); ++i){
            allowedstr += cbox_->itemText(i) + "\n";
        }
        exit_dialog(errorstr + allowedstr);
    }
    cbox_->setCurrentIndex(idx);
}

void
ComboKeywordEntry::add_option(const QString& str)
{
    int idx = cbox_->count();
    cbox_->addItem(str);
    cbox_->setItemData(idx, "No documentation", Qt::ToolTipRole);
    tooltips_.push_back("No documentation");

    if (defval_.size() == 0){
        defval_ = str;
    }
}

void
ComboKeywordEntry::add_option_with_tooltip(const QString& str, const QString& tooltip)
{
    int idx = cbox_->count();
    cbox_->addItem(str);
    if (tooltip.size()){
        cbox_->setItemData(idx, tooltip, Qt::ToolTipRole);
        tooltips_.push_back(tooltip);
    }
    else {
        cbox_->setItemData(idx, "No documentation", Qt::ToolTipRole);
        tooltips_.push_back("No documentation");
    }

    if (defval_.size() == 0){
        defval_ = str;
    }
}

void
ComboKeywordEntry::change_tooltip(int idx)
{
    cbox_->setToolTip(tooltips_[idx]);
}

FakeKeywordEntry::FakeKeywordEntry(const QString &keyname, const QString &keytype,
                                         const QString& defval, const QString& docstring)
    : FactoryKeywordEntry(keyname, keytype, defval, docstring)
{
}

KeywordEntry*
FakeKeywordEntry::clone() const
{
    FakeKeywordEntry* entry = new FakeKeywordEntry(keyname_, keytype_, defval_, docstring_);
    init_clone(entry);
    KeywordEntry::clone_into(entry);
    return entry;
}

YesNoKeywordEntry::YesNoKeywordEntry(const QString &keyname, const QString &keytype,
                                         const QString& defval, const QString& docstring)
    : FactoryKeywordEntry(keyname, keytype, defval, docstring)
{
}

KeywordEntry*
YesNoKeywordEntry::clone() const
{
    YesNoKeywordEntry* entry = new YesNoKeywordEntry(keyname_, keytype_, defval_, docstring_);
    init_clone(entry);
    KeywordEntry::clone_into(entry);
    return entry;
}

bool
YesNoKeywordEntry::included() const
{
    return cbox_->currentIndex() == 0;
}

void
YesNoKeywordEntry::change_tabs(int idx)
{
    if (owner_widget_){
        if (idx == 0){
            activate(owner_widget_);
        }
        else {
            deactivate();
        }
    }
}


FactoryKeywordEntry::FactoryKeywordEntry(const QString &keyname, const QString &keytype,
                                         const QString& defval, const QString& docstring)
    : ComboKeywordEntry(keyname, keytype, defval, docstring),
      owner_widget_(0)
{
}

void
FactoryKeywordEntry::change_tabs(int idx)
{

    if (my_tab_->is_active()){
        current_layout_->deactivate();
    }

    current_layout_ = input_layouts_[idx];
    my_tab_->set_layout(current_layout_);

    if (my_tab_->is_active()){
        current_layout_->activate(owner_widget_);
    }

}

void
FactoryKeywordEntry::include_box_changed(void)
{
    if (owner_widget_){
        if (include_box_->isChecked()){
            activate(owner_widget_);
        }
        else {
            deactivate();
        }
    }
}

QComboBox*
FactoryKeywordEntry::child_combo_box()
{
    QComboBox* cpy = new QComboBox;
    for (int i=0; i < cbox_->count(); ++i){
        QString item_text = cbox_->itemText(i);
        cpy->addItem(item_text);
        cpy->setItemData(i, tooltips_[i], Qt::ToolTipRole);
    }
    cpy->setToolTip(docstring_);
    cpy->setCurrentIndex(cbox_->currentIndex());
    QFont label_font("Arial", Fonts::InputTabSize);
    cpy->setFont(label_font);

    QObject::connect(cpy, SIGNAL(currentIndexChanged(int)), cbox_, SLOT(setCurrentIndex(int)));
    QObject::connect(cbox_, SIGNAL(currentIndexChanged(int)), cpy, SLOT(setCurrentIndex(int)));
    return cpy;
}

void
FactoryKeywordEntry::init_clone(FactoryKeywordEntry* entry) const
{
    for (int i=0; i < cbox_->count(); ++i){
        QString item_text = cbox_->itemText(i);
        entry->add_option_with_tooltip(item_text, tooltips_[i]);
    }
    entry->set_value(cbox_->currentText());
    entry->typenames_ = typenames_;
    QObject::connect(entry->cbox_, SIGNAL(currentIndexChanged(int)), entry, SLOT(change_tabs(int)));
    QObject::connect(entry->include_box_, SIGNAL(stateChanged(int)), entry, SLOT(include_box_changed(void)));
}

KeywordEntry*
FactoryKeywordEntry::clone() const
{
    FactoryKeywordEntry* entry = new FactoryKeywordEntry(keyname_, keytype_, defval_, docstring_);
    init_clone(entry);
    KeywordEntry::clone_into(entry);
    return entry;
}

void
FactoryKeywordEntry::add_typename(const QString &typestr)
{
    typenames_.push_back(typestr);
}

void
FactoryKeywordEntry::reset()
{
    if (my_tab_)
        my_tab_->reset();
    for (int i=0; i < (int) input_layouts_.size(); ++i){
        input_layouts_[i]->reset();
    }
    KeywordEntry::reset();
}

void
FactoryKeywordEntry::activate(QTabWidget* tab_widget)
{
    if (!included()){
        //If I am not included
        //Or I'm a yes/no box set to no
        owner_widget_ = tab_widget;
        return;
    }


    if (!my_tab_){
        QComboBox* clone_box  = child_combo_box();
        my_tab_ = new InputTab(keyname_, keytype_, clone_box);
        if (input_layouts_.size() == 0){
            for (unsigned int i=0; i < typenames_.size(); ++i){
                QString typestr = typenames_[i];
                InputTabLayout* tab = new InputTabLayout(typestr);
                input_layouts_.push_back(tab);
            }
        }
    }

    KeywordEntry::activate(tab_widget);

    int cur_idx = cbox_->currentIndex();
    current_layout_ = input_layouts_[cur_idx];
    my_tab_->set_layout(current_layout_);
    owner_widget_ = my_tab_->activate(tab_widget);
}

void
FactoryKeywordEntry::deactivate()
{
    KeywordEntry::deactivate();
    if (included() && !my_tab_){
        exit_dialog("Included keyword " + keyname_ + " does not have a tab in deactivate");
    }

    if (my_tab_) //might not be active because it isn't included
        my_tab_->deactivate();

}

BooleanKeywordEntry::BooleanKeywordEntry(const QString &keyname, const QString& defval, const QString& docstring)
    : ComboKeywordEntry(keyname, "boolean", defval, docstring)
{
    add_option("true");
    add_option("false");
    if (defval.size()){
        set_value(defval);
    }
    else {
        cbox_->setCurrentIndex(0);
    }
}

KeywordEntry*
BooleanKeywordEntry::clone() const
{
    BooleanKeywordEntry* entry = new BooleanKeywordEntry(keyname_, defval_, docstring_);
    KeywordEntry::clone_into(entry);
    return entry;
}

void
TimestampKeywordEntry::set_value(const QString& strval)
{
    QString unit_str, value_str;
    get_value_and_units(strval, value_str, unit_str);

    bool ok = false;
    double val = value_str.toDouble(&ok);
    if (!ok){
        exit_dialog("Invalid timestamp " + strval + " with units " + unit_str + " and value " + value_str);
    }

    if (unit_str.size() != 0){
        QString clean_unit_str = clean_units(unit_str);
        int idx = timeunits_->findText(clean_unit_str);
        if (idx == -1){
            exit_dialog("Invalid units " + unit_str + " for timestamp in " + strval);
        }
        timeunits_->setCurrentIndex(idx);
    }
    else {
        if (val < 1e-10){
            //set to ps
            val /= 1e-12;
            timeunits_->setCurrentIndex(4);
        }
        else if (val < 1e-7){
            //set to ns
            val /= 1e-9;
            timeunits_->setCurrentIndex(3);
        }
        else if (val < 1e-4){
            //set to us
            val /= 1e-6;
            timeunits_->setCurrentIndex(2);
        }
        else if (val < 1e-1){
            //set to ms
            val /= 1e-3;
            timeunits_->setCurrentIndex(1);
        }
        else {
            timeunits_->setCurrentIndex(0);
        }
    }
    QString str;
    str.sprintf("%.6f", val);
    timeval_->setText(str);
}

TimestampKeywordEntry::TimestampKeywordEntry(const QString &keyname, const QString &defval, const QString& docstring)
    : KeywordEntry(keyname, "timestamp", defval, docstring)
{
    parent_ = new QWidget;
    parent_layout_ = new QHBoxLayout;
    parent_->setLayout(parent_layout_);

    timeval_ = new QTextEdit(parent_);

    timeunits_ = new QComboBox(parent_);
    timeunits_->addItem("s");
    timeunits_->addItem("ms");
    timeunits_->addItem("us");
    timeunits_->addItem("ns");
    timeunits_->addItem("ps");
    timeunits_->setFixedWidth(units_combo_width_);

    set_value(defval);

    parent_layout_->addWidget(timeval_);
    parent_layout_->addWidget(timeunits_);
    parent_layout_->setContentsMargins(0,0,0,0);

    QFont font(Fonts::EntryName, Fonts::EntrySize);
    timeunits_->setFont(font);
    timeval_->setFont(font);
}

void
TimestampKeywordEntry::init_statics()
{
    std::map<QString,QString>& m = clean_units_map_;
    m["PS"] = m["Ps"] = "ps";
    m["NS"] = m["Ns"] = "ns";
    m["US"] = m["Us"] = "us";
    m["MS"] = m["Ms"] = "ms";
    m["S"] = "s";
}

QString
TimestampKeywordEntry::clean_units(const QString &units)
{
    std::map<QString,QString>::const_iterator
            it = clean_units_map_.find(units);
    if (it != clean_units_map_.end()){
        return it->second;
    }
    return units;
}

KeywordEntry*
TimestampKeywordEntry::clone() const
{
    TimestampKeywordEntry* entry = new TimestampKeywordEntry(keyname_, defval_, docstring_);
    KeywordEntry::clone_into(entry);
    return entry;
}

void
FrequencyKeywordEntry::set_value(const QString& strval)
{
    QString unit_str, value_str;
    get_value_and_units(strval, value_str, unit_str);

    bool ok;
    double val = value_str.toDouble(&ok);
    if (!ok){
        QString errorstr = "Invalid number " + strval + " for keyword " + keyname_;
        exit_dialog(errorstr);
    }

    if (unit_str.size() != 0){
        QString clean_unit_str = clean_units(unit_str);
        int idx = frequnits_->findText(clean_unit_str);
        if (idx == -1){
            exit_dialog("Invalid units " + unit_str + " for frequency in " + strval);
        }
        frequnits_->setCurrentIndex(idx);
    }
    else {
        if (val < 1e3){
            //set to hz
            frequnits_->setCurrentIndex(0);
        }
        else if (val < 1e6){
            //set to khz
            val /= 1e3;
            frequnits_->setCurrentIndex(1);
        }
        else if (val < 1e9){
            //set to mhz
            val /= 1e6;
            frequnits_->setCurrentIndex(2);
        }
        else {
            //set to ghz
            val /= 1e9;
            frequnits_->setCurrentIndex(3);
        }
    }


    QString str;
    str.sprintf("%.6f", val);
    freqval_->setText(str);
}

FrequencyKeywordEntry::FrequencyKeywordEntry(const QString &keyname, const QString &defval, const QString& docstring)
    : KeywordEntry(keyname, "frequency", defval, docstring)
{
    parent_ = new QWidget;
    parent_layout_ = new QHBoxLayout;
    parent_->setLayout(parent_layout_);

    freqval_ = new QTextEdit(parent_);

    frequnits_ = new QComboBox(parent_);
    frequnits_->addItem("Hz");
    frequnits_->addItem("KHz");
    frequnits_->addItem("MHz");
    frequnits_->addItem("GHz");
    frequnits_->setFixedWidth(units_combo_width_);

    set_value(defval);

    parent_layout_->addWidget(freqval_);
    parent_layout_->addWidget(frequnits_);
    parent_layout_->setContentsMargins(0,0,0,0);

    QFont font(Fonts::EntryName, Fonts::EntrySize);
    frequnits_->setFont(font);
    freqval_->setFont(font);
}

KeywordEntry*
FrequencyKeywordEntry::clone() const
{
    FrequencyKeywordEntry* entry = new FrequencyKeywordEntry(keyname_, defval_, docstring_);
    KeywordEntry::clone_into(entry);
    return entry;
}

void
FrequencyKeywordEntry::init_statics()
{
    std::map<QString,QString>& m = clean_units_map_;
    m["ghz"] = m["Ghz"] = m["GHZ"] = "GHz";
    m["mhz"] = m["Mhz"] = m["MHZ"] = "MHz";
    m["khz"] = m["Khz"] = m["KHZ"] = "KHz";
    m["hz"] = m["HZ"] = "Hz";
}

QString
FrequencyKeywordEntry::clean_units(const QString &units)
{
    std::map<QString,QString>::const_iterator
            it = clean_units_map_.find(units);
    if (it != clean_units_map_.end()){
        return it->second;
    }
    return units;
}


void
KeywordEntry::get_value_and_units(const QString& strval, QString& value_str, QString& unit_str)
{
    int last_idx = strval.length() - 1;
    bool not_number = true;
    while (not_number && last_idx >= 0)
    {
        // try to find the units at the
        QChar last_char = strval[last_idx];
        not_number = !last_char.isDigit();
        --last_idx;
    }
    //we have overshot - reset to correct idx
    last_idx += 2;

    int value_length = last_idx ;
    value_str = strval.mid(0, value_length);
    int unit_length = strval.length() - last_idx;
    unit_str = strval.mid(last_idx, unit_length);
}

void
ByteLengthKeywordEntry::init_statics()
{
    clean_units_map_["bytes"] = "B";
    clean_units_map_["kbytes"] = "KB";
    clean_units_map_["mbytes"] = "MB";
    clean_units_map_["gbytes"] = "MB";
}

QString
ByteLengthKeywordEntry::clean_units(const QString &units)
{
    std::map<QString,QString>::const_iterator
            it = clean_units_map_.find(units);
    if (it != clean_units_map_.end()){
        return it->second;
    }
    return units;
}

void
ByteLengthKeywordEntry::set_value(const QString& strval)
{
    QString unit_str, value_str;
    get_value_and_units(strval, value_str, unit_str);


    bool ok;
    double val = value_str.toDouble(&ok);
    if (!ok){
        QString errorstr = "Invalid byte length " + strval + " for keyword " + keyname_;
        exit_dialog(errorstr);
    }


    if (unit_str.size() != 0){
        QString clean_unit_str = clean_units(unit_str);
        int idx = units_->findText(clean_unit_str);
        if (idx == -1){
            exit_dialog("Invalid units " + unit_str + " for byte length in " + strval);
        }
        units_->setCurrentIndex(idx);
    }
    else{
        if (val < 1e3){
            //set to B
            units_->setCurrentIndex(0);
        }
        else if (val < 1e6){
            //set to KB
            val /= 1e3;
            units_->setCurrentIndex(1);
        }
        else if (val < 1e9){
            //set to MB
            val /= 1e6;
            units_->setCurrentIndex(2);
        }
        else {
            //set to GB
            val /= 1e9;
            units_->setCurrentIndex(3);
        }
    }

    QString str;
    str.sprintf("%.6f", val);
    val_->setText(str);
}

ByteLengthKeywordEntry::ByteLengthKeywordEntry(
        const QString &keyname,
        const QString &defval,
        const QString& docstring)
    : KeywordEntry(keyname, "byte length", defval, docstring)
{
    parent_ = new QWidget;
    parent_layout_ = new QHBoxLayout;
    parent_->setLayout(parent_layout_);

    val_ = new QTextEdit(parent_);

    units_ = new QComboBox(parent_);
    units_->addItem("B");
    units_->addItem("KB");
    units_->addItem("MB");
    units_->addItem("GB");

    units_->setFixedWidth(units_combo_width_);

    set_value(defval);

    parent_layout_->addWidget(val_);
    parent_layout_->addWidget(units_);
    parent_layout_->setContentsMargins(0,0,0,0);

    QFont font(Fonts::EntryName, Fonts::EntrySize);
    units_->setFont(font);
    val_->setFont(font);
}

KeywordEntry*
ByteLengthKeywordEntry::clone() const
{
    ByteLengthKeywordEntry* entry = new ByteLengthKeywordEntry(keyname_, defval_, docstring_);
    KeywordEntry::clone_into(entry);
    return entry;
}

void
BandwidthKeywordEntry::init_statics()
{
    clean_units_map_["gbytes/sec"] = "GB/s";
    clean_units_map_["mbytes/sec"] = "MB/s";
    clean_units_map_["kbytes/sec"] = "KB/s";
}

QString
BandwidthKeywordEntry::clean_units(const QString &units)
{
    std::map<QString,QString>::const_iterator
            it = clean_units_map_.find(units);
    if (it != clean_units_map_.end()){
        return it->second;
    }
    return units;
}

void
BandwidthKeywordEntry::set_value(const QString& strval)
{
    QString unit_str, value_str;
    get_value_and_units(strval, value_str, unit_str);


    bool ok;
    double val = value_str.toDouble(&ok);
    if (!ok){
        QString errorstr = "Invalid bandwidth " + strval + " for keyword " + keyname_;
        exit_dialog(errorstr);
    }


    if (unit_str.size() != 0){
        QString clean_unit_str = clean_units(unit_str);
        int idx = bwunits_->findText(clean_unit_str);
        if (idx == -1){
            exit_dialog("Invalid units " + unit_str + " for bandwidth in " + strval);
        }
        bwunits_->setCurrentIndex(idx);
    }
    else{
        if (val < 1e3){
            //set to B
            bwunits_->setCurrentIndex(0);
        }
        else if (val < 1e6){
            //set to KB
            val /= 1e3;
            bwunits_->setCurrentIndex(1);
        }
        else if (val < 1e9){
            //set to MB
            val /= 1e6;
            bwunits_->setCurrentIndex(2);
        }
        else {
            //set to GB
            val /= 1e9;
            bwunits_->setCurrentIndex(3);
        }
    }

    QString str;
    str.sprintf("%.6f", val);
    bwval_->setText(str);
}

BandwidthKeywordEntry::BandwidthKeywordEntry(const QString &keyname, const QString &defval, const QString& docstring)
    : KeywordEntry(keyname, "bandwidth", defval, docstring)
{
    parent_ = new QWidget;
    parent_layout_ = new QHBoxLayout;
    parent_->setLayout(parent_layout_);

    bwval_ = new QTextEdit(parent_);

    bwunits_ = new QComboBox(parent_);
    bwunits_->addItem("B/s");
    bwunits_->addItem("KB/s");
    bwunits_->addItem("MB/s");
    bwunits_->addItem("GB/s");
    bwunits_->addItem("Mb/s");
    bwunits_->addItem("Gb/s");

    bwunits_->setFixedWidth(units_combo_width_);

    set_value(defval);

    parent_layout_->addWidget(bwval_);
    parent_layout_->addWidget(bwunits_);
    parent_layout_->setContentsMargins(0,0,0,0);

    QFont font(Fonts::EntryName, Fonts::EntrySize);
    bwunits_->setFont(font);
    bwval_->setFont(font);
}

KeywordEntry*
BandwidthKeywordEntry::clone() const
{
    BandwidthKeywordEntry* entry = new BandwidthKeywordEntry(keyname_, defval_, docstring_);
    KeywordEntry::clone_into(entry);
    return entry;
}

VectorKeywordEntry::VectorKeywordEntry(const QString &keyname, const QString& defval, const QString& docstring)
    : OpenKeywordEntry(keyname, "vector", defval, docstring)
{
}

KeywordEntry*
VectorKeywordEntry::clone() const
{
    VectorKeywordEntry* entry =new VectorKeywordEntry(keyname_, defval_, docstring_);
    KeywordEntry::clone_into(entry);
    return entry;
}

OpenKeywordEntry::OpenKeywordEntry(const QString &keyname, const QString& keytype, const QString &defval, const QString& docstring)
    : KeywordEntry(keyname, keytype, defval, docstring)
{
    textedit_ = new QTextEdit(defval);

    QFont font(Fonts::EntryName, Fonts::EntrySize);
    textedit_->setFont(font);
}

void
OpenKeywordEntry::set_value(const QString &val)
{
    textedit_->setText(val);
}

KeywordEntry*
OpenKeywordEntry::clone() const
{
    OpenKeywordEntry* entry = new OpenKeywordEntry(keyname_, keytype_, defval_, docstring_);
    KeywordEntry::clone_into(entry);
    return entry;
}
