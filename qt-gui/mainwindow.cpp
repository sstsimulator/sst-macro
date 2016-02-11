#include "mainwindow.h"
#include "ui_mainwindow.h"
#include "keywordentry.h"
#include "keywordcls.h"
#include "errors.h"
#include "config.h"
#include "util.h"
#include "fonts.h"
#include <QMessageBox>
#include <QCheckBox>
#include <QFileDialog>
#include <QProcessEnvironment>

void
exit_dialog(const QString& errorstr)
{
    QMessageBox errorBox;
    errorBox.setFixedSize(1500,1500);
    errorBox.critical(0,"Error",errorstr);
    QApplication::exit(1);
}

void
error_dialog(const QString& errorstr)
{
    QMessageBox errorBox;
    errorBox.setFixedSize(1500,1500);
    errorBox.critical(0,"Error",errorstr);
}

QString first_delim("|");
QString second_delim(";");
QString third_delim("/");

static std::list<LoggerParam*> loggers_;

void
MainWindow::init()
{

    QFont tabfont(Fonts::TabName, Fonts::TabSize);
    ui->mainTabWidget->setFont(tabfont);
    ui->appTabWidget->setFont(tabfont);
    ui->launchTabWidget->setFont(tabfont);
    ui->externalTabWidget->setFont(tabfont);
    ui->networkTabWidget->setFont(tabfont);
    ui->statTabWidget->setFont(tabfont);
    ui->nodeTabWidget->setFont(tabfont);


    BandwidthKeywordEntry::init_statics();
    FrequencyKeywordEntry::init_statics();
    TimestampKeywordEntry::init_statics();

    KeywordEntry::init_synonyms();

    // add aliased names
    InputTab::add_aliased_type("sstmac::vis::vis_engine", "Visualization");
    InputTab::add_aliased_type("sstmac::hw::node", "Node");
    InputTab::add_aliased_type("sstmac::hw::nic", "NIC");
    InputTab::add_aliased_type("sstmac::sw::launcher", "Launcher");
    InputTab::add_aliased_type("sstmac::native::manager", "Manager");
    InputTab::add_aliased_type("sstmac::hw::interconnect", "Interconnect");
    InputTab::add_aliased_type("sstmac::hw::topology", "Topology");
    InputTab::add_aliased_type("sstmac::hw::router", "Router");
    InputTab::add_aliased_type("sstmac::hw::memory_model", "Memory");
    InputTab::add_aliased_type("sstmac::hw::output_bw_arbitrator", "Arbiter");
    InputTab::add_aliased_type("sstmac::hw::network_switch", "Switch");
    InputTab::add_aliased_type("sstmac::hw::train_switch", "Switch");
    InputTab::add_aliased_type("sstmac::sw::app", "App");
    InputTab::add_aliased_type("sstmac::sw::allocation_strategy", "Node Allocation");
    InputTab::add_aliased_type("sstmac::sw::index_strategy", "Node Layout");
    InputTab::add_aliased_type("sstmac::sw::mpi_api", "MPI API");
    InputTab::add_aliased_type("sstmac::sw::mpi_implementation", "MPI Protocol");
    InputTab::add_aliased_type("sstmac::sw::mpi_queue", "MPI Queue");
    InputTab::add_aliased_type("sstmac::sw::shmem::api", "SHMEM API");

    InputTab::add_subtab_type("Node", ui->nodeTabWidget);
    InputTab::add_subtab_type("Memory", ui->nodeTabWidget);

    InputTab::add_subtab_type("NIC", ui->networkTabWidget);
    InputTab::add_subtab_type("Interconnect", ui->networkTabWidget);
    InputTab::add_subtab_type("Topology", ui->networkTabWidget);
    InputTab::add_subtab_type("Switch", ui->networkTabWidget);
    InputTab::add_subtab_type("Router", ui->networkTabWidget);

    InputTab::add_subtab_type("Node Allocation", ui->launchTabWidget);
    InputTab::add_subtab_type("Node Layout", ui->launchTabWidget);
    InputTab::add_subtab_type("Launcher", ui->launchTabWidget);

    InputTab::add_subtab_type("App", ui->appTabWidget);
    //InputTab::add_subtab_type("MPI API", ui->appTabWidget);
    //InputTab::add_subtab_type("MPI Protocol", ui->appTabWidget);
    //InputTab::add_subtab_type("MPI Queue", ui->appTabWidget);

    SSTKeywordClass::add_ignored_keyword("node_num");

    SSTKeywordClass::add_default_value("node_name", "simple");
    SSTKeywordClass::add_default_value("nic_name", "train");
    SSTKeywordClass::add_default_value("launch_name", "instant");


    QString config_file(GUI_CONFIGURATION_FILE);
    QFile file(config_file);
    if (!file.open(QIODevice::ReadOnly | QIODevice::Text)){
        QString errorstr = "Invalid input file " + config_file;
        exit_dialog(errorstr);
    }
    QTextStream in(&file);
    while (!in.atEnd()){
        QString line = in.readLine();
        QString type = line.split("/").back().replace(".gui", "");
        SSTKeywordClass* cls = SSTKeywordClass::get(type);
        cls->add_keywords(line);
    }
    file.close();

    // Make sure all the launchers have launch_app_1 and others included
    SSTKeywordClass::init_launcher("sstmac::sw::instantlaunch");
    SSTKeywordClass::init_launcher("sstmac::sw::reallaunch");


    //QSpacerItem* tspacer = new QSpacerItem(0,50,QSizePolicy::Expanding,QSizePolicy::Expanding);
    QSpacerItem* bspacer = new QSpacerItem(0,50,QSizePolicy::Expanding,QSizePolicy::Expanding);
    //ui->statWidget->layout()->addItem(tspacer);
    loggers_.push_back(new LoggerParam("Congestion", "congestion",
                                       "Produces .csv files showing the congestion occuring on each link in the system",
                                       ui->statWidget->layout()));
    loggers_.push_back(new LoggerParam("Spyplot", "spyplot",
                                       "Produces a .csv file showing the number of messages/bytes sent between each node/MPI rank",
                                       ui->statWidget->layout()));
    loggers_.push_back(new LoggerParam("Call Graph", "graphviz",
                                       "Produces a callgrind.out file readable by KCachegrind/QCachegrind\n"
                                       "showing a call graph of where the applications spend the most time",
                                       ui->statWidget->layout()));
    ui->statWidget->layout()->addItem(bspacer);

    KeywordEntry::init_external_edit(ui->externalTextEdit);

    InputTab::set_main_widget(ui->mainTabWidget);

    QComboBox* mbox = new QComboBox;
    QFont label_font("Arial", Fonts::InputTabSize);
    mbox->setFont(label_font);

    mbox->addItem("Native");
    mbox->setToolTip("The discrete event manager for the simulation.");
    mgr_tab_ = new InputTab("manager", "sstmac::native::manager", mbox);
    InputTabLayout* mgr_layout = new InputTabLayout("sstmac::native::manager");
    mgr_tab_->set_layout(mgr_layout);
    mgr_tab_->activate(InputTab::get_main_widget());
    InputTab::get_main_widget()->setCurrentIndex(0);
}

MainWindow::MainWindow(QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::MainWindow)
{
    ui->setupUi(this);

    newAct_ = new QAction(tr("&New"), this);
    newAct_->setShortcuts(QKeySequence::New);
    newAct_->setStatusTip(tr("Start new config with GUI defaults"));
    connect(newAct_, SIGNAL(triggered()), this, SLOT(newFile()));

    openAct_ = new QAction(tr("&Open"), this);
    openAct_->setShortcuts(QKeySequence::Open);
    openAct_->setStatusTip(tr("Open a pre-existing configuration file"));
    connect(openAct_, SIGNAL(triggered()), this, SLOT(openFile()));

    saveAct_ = new QAction(tr("&Save"), this);
    saveAct_->setShortcuts(QKeySequence::Save);
    saveAct_->setStatusTip(tr("Save the current GUI state to a configuration file"));
    connect(saveAct_, SIGNAL(triggered()), this, SLOT(saveFile()));

    fileMenu_ = menuBar()->addMenu(tr("&File"));
    fileMenu_->addAction(newAct_);
    fileMenu_->addAction(openAct_);
    fileMenu_->addAction(saveAct_);

    setWindowTitle("SST/macro");
}

MainWindow::~MainWindow()
{
    delete ui;
}

static QString
get_full_path(const QString& fileName)
{
    //test as absolute path
    if (QFile::exists(fileName))
        return fileName;

    //test as src include path
    QString path = GUI_SRC_INCLUDE_PATH "/" + fileName;
    if (QFile::exists(path))
        return path;

    path = GUI_INSTALL_INCLUDE_PATH "/" + fileName;
    if (QFile::exists(path))
        return path;

    return "";
}

static bool
add_mandatory_keywords(const QString& fileName)
{
    QString fullpath = get_full_path(fileName);

    if (fullpath.size() == 0){
        QString errorstr = "Invalid input file " + fileName;
        error_dialog(errorstr);
        return false;
    }

    QFile file(fullpath);
    if (!file.open(QIODevice::ReadOnly | QIODevice::Text)){
        QString errorstr = "Invalid input file " + fileName;
        error_dialog(errorstr);
        return false;
    }

    QTextStream in(&file);
    while (!in.atEnd()){
        QString line = in.readLine();
        line = line.trimmed();
        if (line.startsWith("include")){
            QString file = line.split(" ")[1].trimmed();
            add_mandatory_keywords(file);
            continue;
        }
        else if (line.startsWith("#")){
            continue; //comment line
        }
        else if (line.size() == 0){
            continue; //empty line
        }
        else if (line.startsWith("**.")){
            line = line.mid(3);
        }

        QStringList line_split = line.split("=");
        if (line_split.size() != 2){
            error_dialog("Invalid input file " + fileName + " at line " + line);
            return false;
        }

        QString keyname = line_split[0].trimmed();
        QString keyval = line_split[1].trimmed();
        KeywordEntry::add_mandatory_value(keyname, keyval);
    }
    file.close();

    return true;
}

void
MainWindow::openFile()
{
    QString home = QProcessEnvironment::systemEnvironment().value("HOME");
    QString fileName = QFileDialog::getOpenFileName(this, "Open Config", home);
    if (fileName == "") //nothing doing
        return;

    bool valid_file = add_mandatory_keywords(fileName);
    if (valid_file){
        InputTab::deactivate_all();
        //mgr_tab_->reset();
        mgr_tab_->activate(InputTab::get_main_widget());
        InputTab::get_main_widget()->setCurrentIndex(0);

        QString external_text(ui->externalTextEdit->toPlainText());
        QTextStream sstr(&external_text);
        sstr << "\n\n";
        const std::map<QString,QString> external_vals = KeywordEntry::get_mandatory_values();
        std::map<QString,QString>::const_iterator it, end = external_vals.end();
        for (it=external_vals.begin(); it != end; ++it){
            sstr << it->first << "=" << it->second << "\n";
        }
        ui->externalTextEdit->setText(external_text);
    }
    //regardless, clean up the mandatory values
    KeywordEntry::clear_mandatory_values();
}

void
MainWindow::saveFile()
{
    QString home = QProcessEnvironment::systemEnvironment().value("HOME");
    QString fileName = QFileDialog::getSaveFileName(this, "Save Config", home);

    QFile file(fileName);
    file.open(QIODevice::WriteOnly | QIODevice::Text);
    QTextStream out(&file);
    InputTab::write_all(out);


    QString logger_str;
    QTextStream logger_sstr(&logger_str);
    bool first = true;
    std::list<LoggerParam*>::const_iterator it, end = loggers_.end();
    for (it=loggers_.begin(); it != end; ++it){
        LoggerParam* logger = *it;
        if (logger->is_active()){
            if (first){
                logger_sstr << "<stats> " << logger->param_name();
                first = false;
            }
            else {
                logger_sstr << " | " << logger->param_name();
            }
        }
    }

    if (logger_str.size() != 0){
        out << "\n";
        out << "# Loggers for runtime statistics\n";
        out << "logger_params=" << logger_str;
        out << "\n\n";
    }

    QString extra_stuff = ui->externalTextEdit->toPlainText();
    out << extra_stuff << "\n\n";

    file.close();
}

void
MainWindow::newFile()
{
    QMessageBox::StandardButton butt = QMessageBox::question(this, "SST/macro Reset", "Do you wish to reset to defaults?");
    if (butt == QMessageBox::Yes){
        InputTab::deactivate_all();
        mgr_tab_->reset();
        mgr_tab_->activate(InputTab::get_main_widget());
        InputTab::get_main_widget()->setCurrentIndex(0);
    }

}
