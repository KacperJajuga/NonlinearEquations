program rownaniaNieliniowe
    
    !Kacper Jajuga, kierunek: informatyka, rok I, semestr II

    real a, b, c, h, przedzial, pomocA, pomocB
    integer n

    open(10,file='results.txt')

    write(*,*) 'Podaj POCZATEK przedzialu dla metody przeszukiwan: '
    read(*,*) a
    write(*,*) 'Podaj KONIEC przedzialu dla metody przeszukiwan: '
    read(*,*) b

    while(b .lt. a) do
        write(*,*) 'Koniec przedzialu nie moze byc mniejszy od poczatku przedzialu!'
        write(*,*) 'Ponownie podaj POCZATEK przedzialu dla metody przeszukiwan: '
        read(*,*) a
        write(*,*) 'Ponownie podaj KONIEC przedzialu dla metody przeszukiwan: '
        read(*,*) b
    end while
    
    write(*,*) 'Podaj przyrost h: '
    read(*,*) h

    while(h .le. 0) do
        write(*,*) 'Przyrost nie moze byc mniejszy lub rowny 0!'
        write(*,*) 'Ponownie podaj przyrost h: '
        read(*,*) h
    end while
    
    write(*,*) 'Podaj ostateczna dlugosc przedzialu: '
    read(*,*) przedzial

    while(przedzial .le. 0) do
        write(*,*) 'Ostateczna dlugosc przedzialu nie moze byc mniejsza lub rowna 0!'
        write(*,*) 'Ponownie podaj ostateczna dlugosc przedzialu: '
        read(*,*) h
    end while
    
    while(przedzial .gt. 1) do
        write(*,*) 'Zostala zadeklarowana liczba wieksza od 1, program obliczylby miejsce zerowe ze zbyt mala dokladnoscia'
        write(*,*) 'Ponownie podaj ostateczna dlugosc przedzialu:  '
        read(*,*) h
    end while
    
    n=0

    while (a.le.b) do
        if(f(a)*f(a+h).lt.1.0e-6) then
          pomocA=a
          pomocB=a+h
          write(*,*) !Linijka oddzielajaca kolejne miejsca zerowe, tylko dla estetyki
          write(*,*) 'Znaleziono miejsce zerowe w przedziale <',pomocA,', ',pomocB,'>'
          write(10,*) !Linijka oddzielajaca kolejne miejsca zerowe, tylko dla estetyki
          write(10,*) 'Znaleziono miejsce zerowe w przedziale <',pomocA,', ',pomocB,'>'
            while(abs(pomocB-pomocA).gt.przedzial)do
                write(*,*) 'Zawezanie przedzialu do <',pomocA,', ',pomocB,'>'
                write(10,*) 'Zawezanie przedzialu do <',pomocA,', ',pomocB,'>'
                c=(pomocA+pomocB)/2
                if(f(pomocA)*f(c).le.1.0e-6) then
                    pomocB=c
                else
                    pomocA=c
                end if
            end while
            write(*,*) 'Miejsce zerowe to: ', c
            write(*,*) 'f(',c,') = ',f(c)
            write(*,*) 'licznik = ',n
            write(10,*) 'Miejsce zerowe to: ', c
            write(10,*) 'f(',c,') = ',f(c)
            write(10,*) 'licznik = ',n
        end if
        n=n+1
        a=a+h
    end while
    
    close(10)

    write(*,*) 'Nacisnij Enter aby zakonczyc program'
    read(*,*)

end program

    function f(x)
        f=2*x**3-9.06843*x**2-72.3753*x+114.834
    end !f(x)