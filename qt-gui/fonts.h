#ifndef FONTS_H
#define FONTS_H

#undef BIG_FONTS

class Fonts {
public:
    static const char* EntryName;
    static const char* TabName;
#ifdef BIG_FONTS
    static const int InputTabSize = 28;
    static const int EntrySize = 24;
    static const int TabSize = 24;
#else
    static const int InputTabSize = 18;
    static const int EntrySize = 14;
    static const int TabSize = 16;
#endif
};

#endif // FONTS_H
