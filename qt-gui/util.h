#ifndef UTIL_H
#define UTIL_H

#include <QDebug>
#include <QtCore/QString>

void exit_dialog(const QString& errorstr);
void error_dialog(const QString& errorstr);

extern QString first_delim;
extern QString second_delim;
extern QString third_delim;

template <class T, class U>
T*
safe_cast(U* item)
{
    T* ret = dynamic_cast<T*>(item);
    if (!ret){
        exit_dialog("failed to dynamic lcast object");
    }
    return ret;
}

#endif // UTIL_H
