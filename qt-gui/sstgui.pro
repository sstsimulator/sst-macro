#-------------------------------------------------
#
# Project created by QtCreator 2013-03-11T20:49:12
#
#-------------------------------------------------

QT       += core gui

greaterThan(QT_MAJOR_VERSION, 4): QT += widgets

TARGET = SSTMacro
TEMPLATE = app


SOURCES += main.cpp\
        mainwindow.cpp \
    keywordcls.cpp \
    fonts.cpp \
    keywordentry.cpp \
    inputtab.cpp

HEADERS  += mainwindow.h \
    inputtab.h \
    errors.h \
    fonts.h \
    util.h \
    keywordentry.h \
    keywordcls.h

FORMS    += mainwindow.ui
