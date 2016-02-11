#ifndef INPUTTAB_H
#define INPUTTAB_H

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
#include <QScrollArea>
#include <set>

class KeywordEntry;

class InputTabLayout
{
    private:
        std::list<KeywordEntry*> entries_;

        QFormLayout* options_layout_;

        QWidget* scroll_widget_;

        QScrollArea* scroll_area_;

        QString name_;

        QLabel* no_params_label_;

    public:
        InputTabLayout(const QString& name);

        void add_entry(KeywordEntry* entry);

        QWidget* widget(){
            return scroll_widget_;
        }

        void write(QTextStream& out);

        QScrollArea* scroll_area(){
            return scroll_area_;
        }

        QString name() const {
            return name_;
        }

        int num_entries() {
            return entries_.size();
        }

        void activate(QTabWidget* owner_widget);

        void deactivate();

        void reset();
};

class InputTab
{
    private:
        static std::map<QString,QString> aliased_types_;

        QTabWidget* owner_widget_;

        QWidget* my_tab_;

        QString keytype_;

        InputTabLayout* options_layout_;

        QComboBox* combo_box_;

        bool active_;

        static std::map<QString,QTabWidget*> sub_widgets_;

        static QTabWidget* main_widget_;

        static std::set<InputTab*> active_tabs_;

    public:
        static const int num_default_tabs = 6;

        InputTab(const QString& keyname, const QString& keytype, QComboBox* box);

        static QTabWidget* get_main_widget() {
            return main_widget_;
        }

        static void set_main_widget(QTabWidget* tab_widget) {
            main_widget_ = tab_widget;
        }

        bool is_active() const {
            return active_;
        }

        void write(QTextStream& out);

        static void write_all(QTextStream& out);

        static void deactivate_all();

        void reset();

        static void add_subtab_type(const QString& keytype, QTabWidget* widget);

        static QTabWidget* get_display_widget(const QString& keytype);

        static QString get_aliased_type(const QString& keytype);

        static void add_aliased_type(const QString& keytype, const QString& alias);

        void set_layout(InputTabLayout* layout);

        QTabWidget* activate(QTabWidget* parent_widget);

        void deactivate();

};


#endif // INPUTTAB_H
