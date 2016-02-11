#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>
#include "keywordentry.h"
#include "inputtab.h"

namespace Ui {
class MainWindow;
}

class MainWindow : public QMainWindow
{
    Q_OBJECT
    
private:
    std::list<KeywordEntry*> entries_;

    QMenu *fileMenu_;
    QAction *newAct_;
    QAction *openAct_;
    QAction *saveAct_;

    InputTab* mgr_tab_;

    void init_launcher(const QString& launcher_name);

public:
    explicit MainWindow(QWidget *parent = 0);
    ~MainWindow();

    void init();

public slots:
    void newFile();

    void saveFile();

    void openFile();

private:
    Ui::MainWindow *ui;
};

#endif // MAINWINDOW_H
