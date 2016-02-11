#ifndef KEYWORDUI_H
#define KEYWORDUI_H

#include <QtCore/QVariant>
#include <QAction>
#include <QApplication>
#include <QButtonGroup>
#include <QComboBox>
#include <QGridLayout>
#include <QFormLayout>
#include <QHBoxLayout>
#include <QHeaderView>
#include <QLabel>
#include <QMainWindow>
#include <QMenuBar>
#include <QSpacerItem>
#include <QStatusBar>
#include <QTabWidget>
#include <QToolBar>
#include <QVBoxLayout>
#include <QTextEdit>
#include <QWidget>
#include <QHBoxLayout>
#include <QCheckBox>

#include "inputtab.h"
#include "fonts.h"

#define QFormLayout QFormLayout

class LoggerParam
{
    private:
        QCheckBox* cbox_;

        QString param_name_;

    public:
        LoggerParam(const QString& param_display,
                    const QString& param_name,
                    const QString& docstring,
                    QLayout* layout);

        QString param_name() const {
            return param_name_;
        }

        bool is_active() const {
            return cbox_->isChecked();
        }
};

class KeywordEntry
    : public QObject
{
    Q_OBJECT
private:
    QLabel* my_label_;
    QWidget* my_widget_;
    QSpacerItem* left_spacer_;
    QSpacerItem* right_spacer_;

protected:
    QCheckBox* include_box_;
    QString docstring_;
    QString keyname_string_;
    QString keytype_string_;
    QString keyname_;
    QString keytype_;
    QString defval_;

    bool is_extra_param_;

#ifdef BIG_FONTS
    static const int units_combo_width_ = 130;
    static const int keyname_length_ = 70;
    static const int keytype_length_ = 40;
#else
    static const int units_combo_width_ = 80;
    static const int keyname_length_ = 40;
    static const int keytype_length_ = 20;
#endif

    static std::map<QString,QString> mandatory_values_;
    static std::map<QString,KeywordEntry*> active_entries_;
    static std::map<QString,std::map<QString,QString> > synonyms_;
    static QTextEdit* external_edit_;

    static QString get_synonym(const QString& keyname, const QString& value);

    void clone_into(KeywordEntry* entry) const;

public:
#ifdef BIG_FONTS
    static const int widget_height = 50;
    static const int widget_width = 300;
    static const int label_width = 300;
    static const int name_width = 500;
#else
    static const int widget_height = 30;
    static const int widget_width = 200;
    static const int label_width = 220;
    static const int name_width = 300;
#endif

    KeywordEntry(const QString& keyname, const QString& keytype,
                 const QString& defval, const QString& docstring);

    void init();

    static void init_synonyms();

    static void get_value_and_units(const QString& input_str,
                                    QString& value_str, QString& unit_str);

    virtual QWidget* get_value_widget() = 0;

    virtual void set_value(const QString& val) = 0;

    static void add_mandatory_value(const QString& keyname, const QString& keyval);

    static const std::map<QString,QString>& get_mandatory_values(){
        return mandatory_values_;
    }

    static void init_external_edit(QTextEdit* text_edit){
        external_edit_ = text_edit;
    }

    static void clear_mandatory_values();

    virtual void reset(){
        set_value(defval_);
    }

    virtual bool is_fake() const {
        return false;
    }

    virtual bool included() const {
        return include_box_->isChecked();
    }

    void set_as_extra(){
        is_extra_param_ = true;
    }

    QWidget* get_widget(){
        return my_widget_;
    }

    QLabel* get_label(){
        return my_label_;
    }

    QString keyname() const {
        return keyname_;
    }

    virtual QString get_string_value() = 0;

    virtual void activate(QTabWidget* owner_widget);

    virtual void deactivate();

    virtual KeywordEntry* clone() const = 0;
};


class ComboKeywordEntry
    : public KeywordEntry
{
        Q_OBJECT

protected:
    QComboBox* cbox_;

    QWidget* get_value_widget(){
        return cbox_;
    }

    std::vector<QString> tooltips_;

public:

    static const int value_index = 0;
    static const int typename_index = 1;
    static const int docstring_index = 2;
    static const int num_entries = 3;

    ComboKeywordEntry(const QString& keyname, const QString& keytype,
                      const QString& defval, const QString& docstring);

    QString get_string_value(){
        return cbox_->currentText();
    }

    void set_value(const QString& val);

    void add_option(const QString& str);

    void add_option_with_tooltip(const QString& str, const QString& tooltip);

    virtual KeywordEntry* clone() const;
public slots:
    void change_tooltip(int idx);

};

class FactoryKeywordEntry
    : public ComboKeywordEntry
{
    Q_OBJECT

    private:
        std::vector<InputTabLayout*> input_layouts_;

        std::vector<QString> typenames_;

        InputTab* my_tab_;

        InputTabLayout* current_layout_;

    protected:
        QTabWidget* owner_widget_;

    protected:
        void init_clone(FactoryKeywordEntry* entry) const;

    public:
        FactoryKeywordEntry(const QString& keyname, const QString& keytype,
                            const QString& defval, const QString& docstring);

        virtual void activate(QTabWidget* owner_widget);

        virtual void deactivate();

        QComboBox* child_combo_box();

        virtual void reset();

        void add_typename(const QString& typestr);

        virtual KeywordEntry* clone() const;

    public slots:
        virtual void change_tabs(int idx);

        void include_box_changed(void);
};

class YesNoKeywordEntry :
    public FactoryKeywordEntry
{

    public:
        YesNoKeywordEntry(const QString& keyname, const QString& keytype,
                         const QString& defval, const QString& docstring);

        KeywordEntry* clone() const;

        bool is_fake() const {
            return true;
        }

        void change_tabs(int idx);

        virtual bool included() const;

};

class FakeKeywordEntry :
    public FactoryKeywordEntry
{

    public:
        FakeKeywordEntry(const QString& keyname, const QString& keytype,
                         const QString& defval, const QString& docstring);


        bool is_fake() const {
            return true;
        }

        KeywordEntry* clone() const;
};

class ByteLengthKeywordEntry :
    public KeywordEntry
{
private:
    QWidget* parent_;
    QHBoxLayout* parent_layout_;
    QTextEdit* val_;
    QComboBox* units_;

    static std::map<QString,QString> clean_units_map_;

protected:
    QWidget* get_value_widget(){
        return parent_;
    }

    static QString clean_units(const QString& units);

public:
    ByteLengthKeywordEntry(const QString &keyname, const QString& defval, const QString& docstring);

    QString get_string_value(){
        return val_->toPlainText() + units_->currentText();
    }

    virtual KeywordEntry* clone() const;

    void set_value(const QString& val);

    static void init_statics();

};

class BandwidthKeywordEntry :
    public KeywordEntry
{
private:
    QWidget* parent_;
    QHBoxLayout* parent_layout_;
    QTextEdit* bwval_;
    QComboBox* bwunits_;

    static std::map<QString,QString> clean_units_map_;

protected:
    QWidget* get_value_widget(){
        return parent_;
    }

    static QString clean_units(const QString& units);

public:
    BandwidthKeywordEntry(const QString &keyname, const QString& defval, const QString& docstring);

    QString get_string_value(){
        return bwval_->toPlainText() + bwunits_->currentText();
    }

    virtual KeywordEntry* clone() const;

    void set_value(const QString& val);

    static void init_statics();

};

class FrequencyKeywordEntry :
    public KeywordEntry
{
private:
    QWidget* parent_;
    QHBoxLayout* parent_layout_;
    QTextEdit* freqval_;
    QComboBox* frequnits_;
    static std::map<QString,QString> clean_units_map_;

protected:
    QWidget* get_value_widget(){
        return parent_;
    }

    static QString clean_units(const QString& units);

public:
    FrequencyKeywordEntry(const QString &keyname, const QString& defval, const QString& docstring);

    QString get_string_value(){
        return freqval_->toPlainText() + frequnits_->currentText();
    }

    virtual KeywordEntry* clone() const;

    void set_value(const QString& val);

    static void init_statics();
};

class TimestampKeywordEntry :
    public KeywordEntry
{
private:
    QWidget* parent_;
    QHBoxLayout* parent_layout_;
    QTextEdit* timeval_;
    QComboBox* timeunits_;
     static std::map<QString,QString> clean_units_map_;

protected:
    QWidget* get_value_widget(){
        return parent_;
    }

    static QString clean_units(const QString& units);

public:
    TimestampKeywordEntry(const QString &keyname, const QString& defval, const QString& docstring);

    QString get_string_value(){
        return timeval_->toPlainText() + timeunits_->currentText();
    }

    virtual KeywordEntry* clone() const;

    void set_value(const QString& val);

    static void init_statics();
};

class BooleanKeywordEntry :
    public ComboKeywordEntry
{
public:
    BooleanKeywordEntry(const QString &keyname, const QString& defval, const QString& docstring);

    void set_true();

    void set_false();

    virtual KeywordEntry* clone() const;

};

class OpenKeywordEntry
    : public KeywordEntry
{

protected:
    QTextEdit* textedit_;
    QWidget* get_value_widget(){
        return textedit_;
    }

public:
    OpenKeywordEntry(const QString& keyname, const QString& keytype,
                     const QString& defval, const QString& docstring);

    QString get_string_value(){
        return textedit_->toPlainText();
    }

    virtual KeywordEntry* clone() const;

    void set_value(const QString& val);
};

class VectorKeywordEntry
    : public OpenKeywordEntry
{
    public:
        VectorKeywordEntry(const QString& keyname, const QString& defval, const QString& docstring);

    virtual KeywordEntry* clone() const;
};

#endif // KEYWORDUI_H
