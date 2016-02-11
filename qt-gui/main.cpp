#include "mainwindow.h"
#include <QApplication>

int main(int argc, char *argv[])
{
    QApplication a(argc, argv);
    QCoreApplication::setApplicationName("DCI");

    MainWindow w;
#ifndef __APPLE__
    QIcon icon(":icons/sstmac.ico");
    w.setWindowIcon(icon);
#endif
    w.show();

    w.init();
    
    return a.exec();
}
