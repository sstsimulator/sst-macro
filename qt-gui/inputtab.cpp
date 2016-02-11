#include "inputtab.h"
#include "keywordentry.h"
#include "fonts.h"
#include "keywordcls.h"
#include <QCheckBox>
#include <QLCDNumber>
#include <QDebug>

QTabWidget* InputTab::main_widget_ = 0;
std::map<QString,QTabWidget*> InputTab::sub_widgets_;
std::map<QString,QString> InputTab::aliased_types_;
std::set<InputTab*> InputTab::active_tabs_;

InputTabLayout::InputTabLayout(const QString &name)
    : options_layout_(0),
      scroll_widget_(0),
      scroll_area_(0),
      name_(name)
{
    options_layout_ = new QFormLayout;
    options_layout_->setContentsMargins(0,25,0,150);
    options_layout_->setFieldGrowthPolicy(QFormLayout::FieldsStayAtSizeHint);
    scroll_widget_ = new QWidget;
    scroll_widget_->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
    scroll_widget_->setLayout(options_layout_);

    scroll_area_ = new QScrollArea;
    scroll_area_->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
    scroll_area_->setWidgetResizable(true);
    scroll_area_->setWidget(scroll_widget_);

    SSTKeywordClass* keycls = SSTKeywordClass::get(name);
    keycls->clone(this);

    if (entries_.size() == 0){
        //no params
        options_layout_->addRow("Class Reads No Parameters", new QLCDNumber(1));
    }
}

void
InputTabLayout::write(QTextStream &out)
{
    std::list<KeywordEntry*>::const_iterator it, end = entries_.end();
    for (it=entries_.begin(); it != end; ++it){
        KeywordEntry* entry = *it;
        if (entry->included()){
            if (entry->is_fake()){
                out << "# ";
            }
            out << entry->keyname() << "=" << entry->get_string_value() << endl;
        }

    }
}

void
InputTabLayout::reset()
{
    std::list<KeywordEntry*>::const_iterator it, end = entries_.end();
    for (it=entries_.begin(); it != end; ++it){
        KeywordEntry* entry = *it;
        entry->reset();
    }
}


void
InputTab::deactivate_all()
{
    std::set<InputTab*> tabs = active_tabs_;
    std::set<InputTab*>::const_iterator it, end = tabs.end();
    for (it=tabs.begin(); it != end; ++it){
        InputTab* tab = *it;
        tab->deactivate();
    }
}

void
InputTab::reset()
{
    if (options_layout_)
        options_layout_->reset();
}

void
InputTab::write_all(QTextStream &out)
{
    std::set<InputTab*>::const_iterator it, end = active_tabs_.end();
    for (it=active_tabs_.begin(); it != end; ++it){
        InputTab* tab = *it;
        tab->write(out);
    }
}

void
InputTabLayout::add_entry(KeywordEntry *entry)
{
    entries_.push_back(entry);
    entry->init();
    options_layout_->addRow(entry->get_label(), entry->get_widget());
    options_layout_->setAlignment(entry->get_label(), Qt::AlignCenter);
}

void
InputTabLayout::activate(QTabWidget* owner_widget)
{
    std::list<KeywordEntry*>::iterator it = entries_.begin(), end = entries_.end();
    for ( ; it != end; ++it){
        KeywordEntry* entry = *it;
        entry->activate(owner_widget);
    }
}

void
InputTabLayout::deactivate()
{
    std::list<KeywordEntry*>::iterator it = entries_.begin(), end = entries_.end();
    for ( ; it != end; ++it){
        KeywordEntry* entry = *it;
        entry->deactivate();
    }
}


InputTab::InputTab(const QString& keyname, const QString& keytype, QComboBox* box)
    : owner_widget_(0),
      my_tab_(0),
      keytype_(get_aliased_type(keytype)),
      options_layout_(0),
      combo_box_(box),
      active_(false)
{
    my_tab_ = new QWidget();
    //my_tab_->setMinimumSize(main_widget_->size());
    //my_tab_->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
    //my_tab_->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);

    QVBoxLayout* tab_layout = new QVBoxLayout;
    my_tab_->setLayout(tab_layout);

    QHBoxLayout* topbar = new QHBoxLayout;
    QWidget* box_widget = new QWidget;
    box_widget->setLayout(topbar);

    QSpacerItem* lspacer = new QSpacerItem(50,0,QSizePolicy::Expanding,QSizePolicy::Minimum);
    QSpacerItem* rspacer = new QSpacerItem(50,0,QSizePolicy::Expanding,QSizePolicy::Minimum);
    combo_box_->setFixedSize(KeywordEntry::widget_width, KeywordEntry::widget_height);
    QLabel* label = new QLabel(keyname);
    label->setToolTip(combo_box_->toolTip());
    QFont label_font("Arial", Fonts::InputTabSize, 1, true);
    label->setFont(label_font);
    topbar->addItem(lspacer);
    topbar->addWidget(label);
    topbar->addWidget(combo_box_, 0, Qt::AlignHCenter);
    topbar->addItem(rspacer);
    tab_layout->addWidget(box_widget);
}

void
InputTab::write(QTextStream& out)
{
    //nothing useful to add
    if (options_layout_->num_entries() == 0)
        return;

    out << "# Parameters for " << keytype_ << endl;
    options_layout_->write(out);
    out << endl;
}

QString
InputTab::get_aliased_type(const QString& keytype)
{
    std::map<QString,QString>::const_iterator it = aliased_types_.find(keytype);
    if (it == aliased_types_.end()){
        return keytype;
    }
    return it->second;
}

void
InputTab::add_aliased_type(const QString &keytype, const QString &alias)
{
    aliased_types_[keytype] = alias;
}

void
InputTab::set_layout(InputTabLayout *layout)
{

    if (options_layout_){
        my_tab_->layout()->removeWidget(options_layout_->scroll_area());
        options_layout_->scroll_area()->setVisible(false);
    }

    my_tab_->layout()->addWidget(layout->scroll_area());
    layout->scroll_area()->setVisible(true);
    options_layout_ = layout;

}

QTabWidget*
InputTab::get_display_widget(const QString &keytype)
{
    std::map<QString,QTabWidget*>::const_iterator it = sub_widgets_.find(keytype);
    if (it == sub_widgets_.end()){
        return 0;
    }
    return it->second;
}

void
InputTab::add_subtab_type(const QString &keytype, QTabWidget *widget)
{
    sub_widgets_[keytype] = widget;
}

QTabWidget*
InputTab::activate(QTabWidget* parent_widget)
{
    if (active_)
        return owner_widget_;

    active_tabs_.insert(this);

    active_ = true;
    owner_widget_ = get_display_widget(keytype_);
    if (owner_widget_ == 0){ //nope, nothing special
        owner_widget_ = parent_widget;
    }

    if (owner_widget_ == main_widget_){
        //I want to insert after all new tabs
        //and before all the default subtabs
        int num_tabs_total = main_widget_->count();
        int num_new_tabs = num_tabs_total - num_default_tabs;
        int insert_idx = num_new_tabs;
        owner_widget_->insertTab(insert_idx, my_tab_, keytype_);
    }
    else {
        owner_widget_->addTab(my_tab_, keytype_);
    }

    options_layout_->activate(owner_widget_);
    return owner_widget_;
}

void
InputTab::deactivate()
{
    if (!active_)
        return;

    active_tabs_.erase(this);
    active_ = false;
    int my_idx = owner_widget_->indexOf(my_tab_);
    owner_widget_->removeTab(my_idx);
    options_layout_->deactivate();
}
