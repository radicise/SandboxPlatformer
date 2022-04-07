#lang "fblite"
#define WIN_INCLUDEALL
#include "fbgfx.bi"
#if __FB_LANG__ = "fb"
Using FB
#endif
#include once "windows.bi"
#inclib "advapi32"
'The levelbrowser function is a tweaked version of a function that was sourced from https://www.freebasic.net/forum/viewtopic.php?t=10981
DIM resX as INTEGER
DIM resY as INTEGER
DIM usernam as STRING * 256
DIM nameBuffer as LONG
DIM errorCode as INTEGER
DIM Shared as INTEGER k
DIM platforms as INTEGER
DIM shared as INTEGER gamemode
Input "input desired window width (must be at least 640): ",resX
Input "input desired window height (must be at least 480): ",resY
Input "input desired amount of platforms (must be from 20 to 400): ",platforms
Input "input game mode (0=sandbox, 2=sidescroll): ",gamemode
If gamemode<>2 THEN gamemode=0
platforms=int(platforms)
If platforms<20 THEN platforms=20
If platforms>400 THEN platforms=400
resX=int(resX)
resY=int(resY)
If resX<640 THEN resX=640
If resY<480 THEN resY=480
Cls
PRINT "resolution: ";resX;"x";resY
Screenres(resX,resY)
PRINT "platforms: ";platforms
DIM mouseX as INTEGER
DIM mouseY as INTEGER
DIM whSt as INTEGER
DIM btSt as INTEGER
DIM clSt as INTEGER
DIM Shared as INTEGER X
DIM Shared as INTEGER Y
DIM Shared as INTEGER Z
DIM Shared as INTEGER W1
DIM Shared as INTEGER W2
DIM Shared as INTEGER W3
DIM Shared as INTEGER W4
DIM Shared as INTEGER X2
DIM Shared as INTEGER Y2
DIM Shared as INTEGER Z2
DIM E as INTEGER
DIM Shared as INTEGER A
DIM times as LONG
DIM sleeper as DOUBLE
DIM Shared as INTEGER graved
DIM Shared as INTEGER B
DIM Shared as INTEGER F
DIM Shared as STRING W
DIM Shared as INTEGER velX
DIM Shared as INTEGER velY
DIM Shared as INTEGER jumpTime
DIM Shared as INTEGER fallTime
DIM Shared as INTEGER upY
DIM Shared as INTEGER downY
DIM Shared as INTEGER S
DIM q as INTEGER
DIM blocks(20,(0-2) to 399) as INTEGER
DIM whStStored as INTEGER
DIM wheelSizer as INTEGER
DIM bigger as INTEGER
DIM ZStored as INTEGER
DIM g as INTEGER
DIM selection as INTEGER
DIM doubleSel as DOUBLE
DIM dispLogic as INTEGER
DIM switchUse as INTEGER
DIM switchUseOld as INTEGER
DIM M as INTEGER
DIM H as INTEGER
DIM downH as INTEGER
DIM downHOld as INTEGER
DIM I as INTEGER
DIM downI as INTEGER
DIM downIOld as INTEGER
DIM O as INTEGER
DIM downO as INTEGER
DIM downOOld as INTEGER
DIM currentFilename as STRING
DIM Shared as STRING show
DIM Shared as INTEGER showNumb
DIM printX as INTEGER
DIM printY as INTEGER
DIM V as INTEGER
DIM downV as INTEGER
DIM downVOld as INTEGER
DIM frameDisplay(0 to 1,0 to 30) as DOUBLE
DIM count1 as INTEGER
DIM count2 as INTEGER
DIM lineX as INTEGER
DIM lineY as INTEGER
DIM firstAdd as INTEGER
DIM secondAdd as INTEGER
DIM items as INTEGER
DIM P as BYTE
DIM downP as BYTE
DIM downPOld as BYTE
DIM Shared as INTEGER autoJump
DIM username as STRING
DIM Shared as INTEGER isAutoJump
DIM downOne as BYTE
DIM downOneOld as BYTE
DIM downThree as BYTE
DIM downThreeOld as BYTE
DIM Shared as INTEGER frameCounter
DIM storedX as INTEGER
DIM storedY as INTEGER
DIM Shared as BYTE goalReachedOld
DIM goalTime as INTEGER
DIM Shared as BYTE cycleNumb
DIM Shared as BYTE goal
DIM Shared as BYTE upTrue
DIM Shared as INTEGER newYPos
DIM Shared as BYTE blockHit
DIM dynCam as BYTE
DIM downSix as BYTE
DIM downSixOld as BYTE
DIM downSeven as BYTE
DIM downSevenOld as BYTE
If gamemode=2 THEN dynCam=1
firstAdd=25
blocks(0,(0-2))=00000008
blocks(1,(0-2))=platforms
showNumb=300
currentFileName=""
Function levelbrowser(Byref ititle As String, Byref idir As String = CurDir) As String
  Dim ofn As OPENFILENAME
  Dim filename As Zstring * (MAX_PATH + 1)
  Dim title As Zstring * 32 => ititle
  Dim initialdir As Zstring * 256 => idir
  With ofn
    .lStructSize       = Sizeof(OPENFILENAME)
    .hwndOwner         = NULL
    .hInstance         = GetModuleHandle(NULL)
    .lpstrFilter       = Strptr(!"All Files, (*.*)\0*.*\0")
    .lpstrCustomFilter = NULL
    .nMaxCustFilter    = 0
    .nFilterIndex      = 1
    .lpstrFile         = @filename
    .nMaxFile          = Sizeof(filename)
    .lpstrFileTitle    = NULL
    .nMaxFileTitle     = 0
    .lpstrInitialDir   = @initialdir
    .lpstrTitle        = @title
    .Flags = OFN_EXPLORER Or OFN_PATHMUSTEXIST Or OFN_HIDEREADONLY Or OFN_NOCHANGEDIR
    .nFileOffset       = 0
    .nFileExtension    = 0
    .lpstrDefExt       = NULL
    .lCustData         = 0
    .lpfnHook          = NULL
    .lpTemplateName    = NULL
  End With
  If (GetOpenFileName(@ofn) = FALSE) Then Return ""
  Return filename
End Function
sub jump()
    If jumpTime=31 THEN autoJump=0
    If jumpTime<31 THEN
        If S=17 or autoJump=1 THEN jumpTime=jumpTime+1
    End If
    If jumpTime=55 THEN jumpTime=88
    If S<>17 and jumpTime<31 and autoJump=0 and jumpTime<>0 THEN jumpTime=30
    If jumpTime>29 and jumpTime<88 THEN jumpTime=jumpTime+1
    If jumpTime>0 and jumpTime<13 THEN Y=Y-1
    If jumpTime>0 and jumpTime<21 THEN Y=Y-1
    If jumpTime>0 and jumpTime<29 THEN Y=Y-1
    If jumpTime>0 and jumpTime<37 THEN Y=Y-1
    If jumpTime>0 and jumpTime<45 THEN Y=Y-1
    If jumpTime>0 and jumpTime<56 THEN Y=Y-1
END SUB
sub collision(ByRef first as INTEGER,ByRef last as INTEGER)
    A=first
    While A<(last+1)
        W1=1
        W2=1
        W3=1
        W4=1
        If (X-X2)<=(Z+Z2) THEN W1=0
        If (Y-Y2)<=(Z+Z2) THEN W2=0
        If (X2-X)<=(Z+Z2) THEN W3=0
        If (Y2-Y)<=(Z+Z2) THEN W4=0
        If W1=0 and W2=0 and W3=0 and W4=0 THEN
            If A=0 THEN
                If Y<=Y2 and (X-X2)<>(Z+Z2) and (X2-X)<>(Z+Z2) THEN
                    upTrue=1
                    newYPos=Y2-Z2-Z-1
                    If autojump=0 THEN jumpTime=0
                    If isAutoJump=1 THEN
                        If velY>1 THEN autoJump=1
                        If velY>1 THEN jumpTime=29
                        If velY>2 THEN jumpTime=19
                    END IF
                END IF
            END IF
            If A=1 THEN
                If Y>Y2 and (X-X2)<>(Z+Z2) and (X2-X)<>(Z+Z2) THEN
                    Y=Y2+Z2+Z
                    jumpTime=88
                    fallTime=0
                    autoJump=0
                END IF
            END IF
            If A=2 THEN
                If X<=X2 and (Y-Y2)<>(Z+Z2) and (Y2-Y)<>(Z+Z2) THEN X=X-1
                If gamemode=2 and k<>1 THEN blockHit=1
            END IF
            If A=3 THEN
                If X>X2 and (Y-Y2)<>(Z+Z2) and (Y2-Y)<>(Z+Z2) THEN X=X+1
            END IF
            If k=1 THEN
                If goalReachedOld=0 THEN
                    goalTime=frameCounter
                    showNumb=0
                    show="goal reached in "+str(frameCounter)+" frames from last location load"
                    If gamemode=2 THEN show="goal reached in "+str(frameCounter)+" frames"
                End IF
                goal=1
            End If
        END IF
        A=A+1
    WEND
END SUB
sub gravity()
    If velY=0 and fallTime<>0 and (jumpTime=0 or jumpTime=88) THEN fallTime=0
    If fallTime<>45 THEN fallTime=fallTime+1
    Y=Y+1
    If jumpTime=0 or jumpTime=88 THEN
        If fallTime=1 THEN jumpTime=88
        If fallTime>20 THEN Y=Y+1
        If fallTime>35 THEN Y=Y+1
        If fallTime>45 THEN Y=Y+1
    END IF
END SUB
nameBuffer=256
GetUserName(usernam,@nameBuffer)
username=usernam
errorCode=err
If errorCode<>0 THEN GOTO errorHandling
Mkdir("C:\Users\" + username + "\SandboxPlatformer\")
Mkdir("C:\Users\" + username + "\SandboxPlatformer\levels\")
selection=(0-1)
q=0
WHILE q<platforms
    blocks(0,q)=24*(q-(20*int(q/20)))+20
    blocks(1,q)=460-80*(int(q/20))
    blocks(2,q)=10
    blocks(4,q)=5
    q=q+1
WEND
X=22
Y=400
Z=5
fps=0
sleeper=13
frame=0 
jumpTime=88
wheelSizer=0
blocks(5,(0-1))=X
blocks(6,(0-1))=Y
blocks(7,(0-1))=Z
blocks(8,(0-1))=jumpTime
blocks(9,(0-1))=wheelSizer
blocks(10,(0-1))=fallTime
blocks(11,(0-1))=velX
blocks(12,(0-1))=velY
blocks(13,(0-1))=autoJump
blocks(14,(0-1))=storedX
blocks(15,(0-1))=storedY
DISP:
ScreenLock
Cls
If dynCam=0 THEN
    dispLogic=0
    WHILE dispLogic<platforms
        LINE(blocks(0,dispLogic)-blocks(2,dispLogic),(blocks(1,dispLogic)-blocks(2,dispLogic)))-((blocks(0,dispLogic)+blocks(2,dispLogic)),(blocks(1,dispLogic)+blocks(2,dispLogic))),7,BF
        If blocks(5,displogic)=0 THEN LINE(blocks(0,dispLogic)-blocks(2,dispLogic),(blocks(1,dispLogic)-blocks(2,dispLogic)))-((blocks(0,dispLogic)+blocks(2,dispLogic)),(blocks(1,dispLogic)+blocks(2,dispLogic))),113,B
        If blocks(5,displogic)=1 THEN LINE(blocks(0,dispLogic)-blocks(2,dispLogic),(blocks(1,dispLogic)-blocks(2,dispLogic)))-((blocks(0,dispLogic)+blocks(2,dispLogic)),(blocks(1,dispLogic)+blocks(2,dispLogic))),69,B
        If I=0 THEN
            printY=int((blocks(1,dispLogic)/8)-1)
            printX=int((blocks(0,dispLogic)/8)-1)
            If printY<1 THEN printY=1
            If printX<1 THEN printX=1
            If printY>int(resY/8)-2 THEN printY=int(resY/8)-2
            If printX>int(resX/8)-1 THEN printX=int(resX/8)-1
            Locate(printY,printX)
            PRINT dispLogic
        END IF
        dispLogic=dispLogic+1
    WEND
    If I=0 THEN
        printY=int((Y/8)-1)
        printX=int((X/8)-1)
        If printY<1 THEN printY=1
        If printX<1 THEN printX=1
        If printY>int(resY/8)-2 THEN printY=int(resY/8)-2
        If printX>int(resX/8)-1 THEN printX=int(resX/8)-1
        Locate(printY,printX)
        PRINT (0-1)
    END IF
    dispLogic=1
    LINE(blocks(0,dispLogic)-blocks(2,dispLogic),(blocks(1,dispLogic)-blocks(2,dispLogic)))-((blocks(0,dispLogic)+blocks(2,dispLogic)),(blocks(1,dispLogic)+blocks(2,dispLogic))),36,BF
    If blocks(5,displogic)=0 THEN LINE(blocks(0,dispLogic)-blocks(2,dispLogic),(blocks(1,dispLogic)-blocks(2,dispLogic)))-((blocks(0,dispLogic)+blocks(2,dispLogic)),(blocks(1,dispLogic)+blocks(2,dispLogic))),113,B
    If blocks(5,displogic)=1 THEN LINE(blocks(0,dispLogic)-blocks(2,dispLogic),(blocks(1,dispLogic)-blocks(2,dispLogic)))-((blocks(0,dispLogic)+blocks(2,dispLogic)),(blocks(1,dispLogic)+blocks(2,dispLogic))),69,B
    LINE(X-Z,Y-Z)-(X+Z,Y+Z),5,BF
    If selection<>(0-1) THEN LINE(blocks(0,selection)-blocks(2,selection),((blocks(1,selection))-(blocks(2,selection))))-((blocks(0,selection)+blocks(2,selection)),(blocks(1,selection)+blocks(2,selection))),89,BF
    If selection=(0-1) THEN LINE(X-Z,Y-Z)-(X+Z,Y+Z),38,BF
    If I=0 THEN
        printY=int((blocks(1,1)/8)-1)
        printX=int((blocks(0,1)/8)-1)
        If printY<1 THEN printY=1
        If printX<1 THEN printX=1
        If printY>int(resY/8)-2 THEN printY=int(resY/8)-2
        If printX>int(resX/8)-1 THEN printX=int(resX/8)-1
        Locate(printY,printX)
        PRINT "goal"
    END IF
    If I=0 THEN
        printY=int((blocks(1,selection)/8)-1)
        printX=int((blocks(0,selection)/8)-1)
        If printY<1 THEN printY=1
        If printX<1 THEN printX=1
        If printY>int(resY/8)-2 THEN printY=int(resY/8)-2
        If printX>int(resX/8)-1 THEN printX=int(resX/8)-1
        Locate(printY,printX)
        PRINT selection
    END IF
    LINE(X-Z,Y-Z)-(X+Z,Y+Z),113,B
    If O=0 THEN LINE(X,Y)-(X+30*velX,Y+30*velY),15
End If
If dynCam=1 THEN
        dispLogic=0
    WHILE dispLogic<platforms
        LINE((blocks(0,dispLogic)-blocks(2,dispLogic))-(X-(0.5)*resX),((blocks(1,dispLogic)-blocks(2,dispLogic)))-(Y-(0.5)*resY))-(((blocks(0,dispLogic)+blocks(2,dispLogic)))-(X-(0.5)*resX),((blocks(1,dispLogic)+blocks(2,dispLogic)))-(Y-(0.5)*resY)),7,BF
        If blocks(5,displogic)=0 THEN LINE((blocks(0,dispLogic)-blocks(2,dispLogic))-(X-(0.5)*resX),((blocks(1,dispLogic)-blocks(2,dispLogic)))-(Y-(0.5)*resY))-(((blocks(0,dispLogic)+blocks(2,dispLogic)))-(X-(0.5)*resX),((blocks(1,dispLogic)+blocks(2,dispLogic)))-(Y-(0.5)*resY)),113,B
        If blocks(5,displogic)=1 THEN LINE((blocks(0,dispLogic)-blocks(2,dispLogic))-(X-(0.5)*resX),((blocks(1,dispLogic)-blocks(2,dispLogic)))-(Y-(0.5)*resY))-(((blocks(0,dispLogic)+blocks(2,dispLogic)))-(X-(0.5)*resX),((blocks(1,dispLogic)+blocks(2,dispLogic)))-(Y-(0.5)*resY)),69,B
        If I=0 THEN
            printY=int(((blocks(1,dispLogic)-(Y-(0.5)*resY))/8)-1)
            printX=int(((blocks(0,dispLogic)-(X-(0.5)*resX))/8)-1)
            If printY<1 THEN printY=1
            If printX<1 THEN printX=1
            If printY>int(resY/8)-2 THEN printY=int(resY/8)-2
            If printX>int(resX/8)-1 THEN printX=int(resX/8)-1
            Locate(printY,printX)
            PRINT dispLogic
        END IF
        dispLogic=dispLogic+1
    WEND
    If I=0 THEN
        printY=int((0.5*resY/8)-1)
        printX=int((0.5*resX/8)-1)
        If printY<1 THEN printY=1
        If printX<1 THEN printX=1
        If printY>int(resY/8)-2 THEN printY=int(resY/8)-2
        If printX>int(resX/8)-1 THEN printX=int(resX/8)-1
        Locate(printY,printX)
        PRINT (0-1)
    END IF
    dispLogic=1
    LINE((blocks(0,dispLogic)-blocks(2,dispLogic))-(X-(0.5)*resX),((blocks(1,dispLogic)-blocks(2,dispLogic)))-(Y-(0.5)*resY))-(((blocks(0,dispLogic)+blocks(2,dispLogic)))-(X-(0.5)*resX),((blocks(1,dispLogic)+blocks(2,dispLogic)))-(Y-(0.5)*resY)),36,BF
    If blocks(5,displogic)=0 THEN LINE((blocks(0,dispLogic)-blocks(2,dispLogic))-(X-(0.5)*resX),((blocks(1,dispLogic)-blocks(2,dispLogic)))-(Y-(0.5)*resY))-(((blocks(0,dispLogic)+blocks(2,dispLogic)))-(X-(0.5)*resX),((blocks(1,dispLogic)+blocks(2,dispLogic)))-(Y-(0.5)*resY)),113,B
    If blocks(5,displogic)=1 THEN LINE((blocks(0,dispLogic)-blocks(2,dispLogic))-(X-(0.5)*resX),((blocks(1,dispLogic)-blocks(2,dispLogic)))-(Y-(0.5)*resY))-(((blocks(0,dispLogic)+blocks(2,dispLogic)))-(X-(0.5)*resX),((blocks(1,dispLogic)+blocks(2,dispLogic)))-(Y-(0.5)*resY)),69,B
    LINE(0.5*resX-Z,0.5*resY-Z)-(0.5*resX+Z,0.5*resY+Z),5,BF
    If selection<>(0-1) THEN LINE((blocks(0,selection)-blocks(2,selection))-(X-(0.5)*resX),((blocks(1,selection)-blocks(2,selection)))-(Y-(0.5)*resY))-(((blocks(0,selection)+blocks(2,selection)))-(X-(0.5)*resX),((blocks(1,selection)+blocks(2,selection)))-(Y-(0.5)*resY)),89,BF
    If selection=(0-1) THEN LINE(0.5*resX-Z,0.5*resY-Z)-(0.5*resX+Z,0.5*resY+Z),38,BF
    If I=0 THEN
        printY=int(((blocks(1,1)-(Y-(0.5)*resY))/8)-1)
        printX=int(((blocks(0,1)-(X-(0.5)*resX))/8)-1)
        If printY<1 THEN printY=1
        If printX<1 THEN printX=1
        If printY>int(resY/8)-2 THEN printY=int(resY/8)-2
        If printX>int(resX/8)-1 THEN printX=int(resX/8)-1
        Locate(printY,printX)
        PRINT "goal"
    END IF
    If I=0 THEN
        printY=int(((blocks(1,selection)-(Y-(0.5)*resY))/8)-1)
        printX=int(((blocks(0,selection)-(X-(0.5)*resX))/8)-1)
        If printY<1 THEN printY=1
        If printX<1 THEN printX=1
        If printY>int(resY/8)-2 THEN printY=int(resY/8)-2
        If printX>int(resX/8)-1 THEN printX=int(resX/8)-1
        Locate(printY,printX)
        PRINT selection
    END IF
    LINE(0.5*resX-Z,0.5*resY-Z)-(0.5*resX+Z,0.5*resY+Z),113,B
    If O=0 THEN LINE(0.5*resX,0.5*resY)-(0.5*resX+30*velX,0.5*resY+30*velY),15
END IF
Locate(1,1)
If H=0 THEN
    If gamemode=0 THEN PRINT "GAME MODE: sandbox"
    If gamemode=2 THEN PRINT "GAME MODE: sidescroll"
    PRINT ""
    PRINT "A to move left"
    PRINT "D to move left"
    PRINT "W, RIGHT BUTTON, or SPACE to jump (hold jump longer for a larger jump)"
    PRINT "TAB to cycle selection forward"
    PRINT "LSHIFT + TAB to cycle selection backward"
    PRINT "LEFT BUTTON to move selected item (movement changes when dynamic camera is on)"
    PRINT "SCROLL WHEEL to change size of selected item"
    PRINT "P to toggle platform bounce"
    PRINT "1 to save player location"
    PRINT "3 to load player location"
    PRINT "ESCAPE to exit"
    PRINT ""
    PRINT "selected item:"
    PRINT "    number= "; selection
    PRINT "      type= ";
    If selection>(0-1) THEN PRINT "platform"
    If selection=(0-1) THEN PRINT "player"
    PRINT "X position= "; blocks(0,selection)
    PRINT "Y position= "; blocks(1,selection)
    PRINT "      size= "; blocks(2,selection)
    If selection=(0-1) THEN PRINT "X velocity= "; velX
    If selection=(0-1) THEN PRINT "Y velocity= "; velY
    If selection<>(0-1) and blocks(5,selection)=0 THEN PRINT "    Bouncy: No "
    If selection<>(0-1) and blocks(5,selection)=1 THEN PRINT "    Bouncy: Yes"
    If selection<>(0-1) THEN PRINT ""
    PRINT ""
    PRINT ""
    PRINT "v0.0.0.8"
    PRINT ""
    PRINT ""
    PRINT ""
    PRINT ""
    PRINT ""
    PRINT ""
    PRINT ""
    PRINT ""
    PRINT ""
    PRINT ""
    Print ""
END IF
PRINT "H to show / hide help"
PRINT "I to show / hide item IDs"
PRINT "O to show / hide player path"
PRINT "R to save level"
PRINT "T to load level"
PRINT "V to show / hide debug"
PRINT "6 to toggle gamemode"
PRINT "7 to toggle dynamic camera"
If V=0 THEN
    Line(635,260)-(403,260)
    Line(635,270)-(403,270)
    Line(635,280)-(403,280)
    Line(635,290)-(403,290)
    Line(635,300)-(403,300)
    Line(635,345)-(403,345)
    Line(635,350)-(403,350)
    Line(635,370)-(403,370)
    Line(635,390)-(403,390)
    Line(635,410)-(403,410)
    Line(635,430)-(403,430)
    Locate(33,49)
    PRINT "40"
    Locate(36,49)
    PRINT "60"
    Locate(38,49)
    PRINT "80"
    Locate(43,50)
    PRINT "1"
    Locate(45,50)
    PRINT "2"
    Locate(47,50)
    PRINT "6"
    Locate(49,49)
    PRINT "10"
    Locate(52,49)
    PRINT "14"
    Locate(54,49)
    PRINT "18"
    Locate(30,65)
    PRINT "fps"
    Locate(42,56)
    PRINT "frame pause time (ms)"
    Locate(22,18)
    PRINT "frames so far this second ";frameSec
    Locate(23,40)
    PRINT "fps ";fps;" (should settle at around 60)"
    Locate(24,22)
    PRINT "frame pause time (ms) ";sleeper
    Locate(25,38)
    PRINT "frame ";frame
    Locate(26,18)
    PRINT "platform loads last frame ";items
    Locate(27,12)
    Print "frames since last location load ";frameCounter
    Locate(28,41)
    PRINT "jt ";jumpTime
    Locate(29,41)
    PRINT "ft ";fallTime
    Locate(30,40)
    PRINT "bgr ";bigger
    Locate(31,39)
    PRINT "whSt ";whSt
    Locate(32,41)
    PRINT "aJ ";autoJump
    Locate(33,41)
    PRINT "uT ";upTrue
    count1=0
    While count1<2
        count2=0
        While count2<30
            lineY2=lineY
            lineX2=lineX
            lineY=frameDisplay(count1,count2)
            lineX=count2+1
            If count1=0 THEN lineX=lineX+firstAdd
            If count1=1 THEN lineX=lineX+secondAdd
            If lineX>29 THEN lineX=lineX-30
            If lineX<>0 and count2<>0 THEN LINE((635-(lineX*8)),(((120*count1)+220)+lineY))-((635-(lineX2*8)),(((120*count1)+220)+lineY2)),10
            count2=count2+1
        WEND
        count2=29
        lineY=frameDisplay(count1,count2)
        lineX=0
        If count1=0 THEN lineX=lineX+firstAdd
        If count1=1 THEN lineX=lineX+secondAdd
        If lineX>29 THEN lineX=lineX-30
        count2=0
        lineY2=lineY
        lineX2=lineX
        lineY=frameDisplay(count1,count2)
        lineX=count2+1
        If count1=0 THEN lineX=lineX+firstAdd
        If count1=1 THEN lineX=lineX+secondAdd
        If lineX>29 THEN lineX=lineX-30
        If lineX<>0 THEN LINE((635-(lineX*8)),(((120*count1)+220)+lineY))-((635-(lineX2*8)),(((120*count1)+220)+lineY2)),10
        count1=count1+1
    WEND
END IF
If showNumb<300 and showNumb-(40*(int(showNumb/40)))<20 THEN
    Locate(45,15)
    PRINT show
END IF
ScreenUnlock
times=TIMER
frame=frame+1
frameCounter=frameCounter+1
If times=times2 THEN frameSec=frameSec+1
If times<>times2 THEN fps=frameSec
If times<>times2 THEN frameSec=1
If times<>times2 THEN
    firstAdd=firstAdd+1
    If firstAdd=30 THEN firstAdd=0
    frameDisplay(0,29-firstAdd)=fps
    times2=times
    WHILE len(inkey)<>0
    WEND
END IF
If fps>60 THEN sleeper=sleeper+0.01
If fps<60 THEN sleeper=sleeper-0.01
If sleeper<1 THEN sleeper=1
secondAdd=secondAdd+1
If secondAdd=30 THEN secondAdd=0
frameDisplay(1,29-secondAdd)=sleeper*5
S=0
F=0
W=""
M=0
SLEEP (int(sleeper))
getMouse(mouseX,mouseY,whST,g,clSt)
If g=1 or g=3 or g=5 or g=7 THEN M=18
F=0
If Multikey(SC_T)=(0-1) THEN GOTO fileLoad
loadReturn:
If Multikey(SC_R)=(0-1) THEN GOTO fileSave
saveReturn:
If Multikey(SC_W)=(0-1) or Multikey(SC_SPACE)=(0-1) THEN S=17
switchUse=0
If Multikey(SC_TAB)=(0-1) THEN
    switchUse=1
    If switchUseOld=0 THEN
        selection=selection+1
        If selection>=platforms THEN selection=(0-1)
    END IF
    If Multikey(SC_LSHIFT)=(0-1) THEN
        If switchUseOld=0 THEN
            selection=selection-1
            If selection<(0-1) THEN selection=platforms-1
        END IF
    END IF
END IF
If Multikey(SC_LSHIFT)=(0-1) and Multikey(SC_TAB)=(0-1) THEN
    If switchUseOld=0 THEN
        selection=selection-1
        If selection<(0-1) THEN selection=platforms-1
    END IF
END IF
switchUseOld=switchUse
If Multikey(SC_A)=(0-1) THEN W="a"
If W<>"a" THEN
    If Multikey(SC_D)=(0-1) THEN W="d"
END IF
If W="a" THEN
    If Multikey(SC_D)=(0-1) THEN W=""
END IF
If Multikey(SC_ESCAPE)=(0-1) THEN W="Esc"
If g=2 or g=3 or g=6 or g=7 THEN S=17
downO=0
If Multikey(SC_O)=(0-1) THEN downO=1
If downOOld=0 and downO=1 THEN
    If O=0 THEN O=2
    If O=1 THEN O=0
    If O=2 THEN O=1
END IF
downOOld=downO
downV=0
If Multikey(SC_V)=(0-1) THEN downV=1
If downVOld=0 and downV=1 THEN
    If V=0 THEN V=2
    If V=1 THEN V=0
    If V=2 THEN V=1
END IF
downVOld=downV
downP=0
If Multikey(SC_P)=(0-1) THEN downP=1
If downPOld=0 and downP=1 THEN
    If selection<>(0-1) THEN
        If blocks(5,selection)=0 THEN blocks(5,selection)=2
        If blocks(5,selection)=1 THEN blocks(5,selection)=0
        If blocks(5,selection)=2 THEN blocks(5,selection)=1
    END IF
END IF
downPOld=downP
downI=0
If Multikey(SC_I)=(0-1) THEN downI=1
If downIOld=0 and downI=1 THEN
    If I=0 THEN I=2
    If I=1 THEN I=0
    If I=2 THEN I=1
END IF
downIOld=downI
downH=0
If Multikey(SC_H)=(0-1) THEN downH=1
If downHOld=0 and downH=1 THEN
    If H=0 THEN H=2
    If H=1 THEN H=0
    If H=2 THEN H=1
END IF
downHOld=downH
downOne=0
If Multikey(SC_3)=(0-1) THEN downOne=1
If downOneOld=0 and downOne=1 THEN
    X=blocks(5,(0-1))
    Y=blocks(6,(0-1))
    Z=blocks(7,(0-1))
    frameCounter=0
    jumpTime=blocks(8,(0-1))
    wheelSizer=blocks(9,(0-1))
    fallTime=blocks(10,(0-1))
    velX=blocks(11,(0-1))
    velY=blocks(12,(0-1))
    autoJump=blocks(13,(0-1))
    storedX=blocks(14,(0-1))
    storedY=blocks(15,(0-1))
    showNumb=280
    show="loaded player location"
END IF
downOneOld=downOne
If blockHit=1 THEN
    X=blocks(5,(0-1))
    Y=blocks(6,(0-1))
    Z=blocks(7,(0-1))
    frameCounter=0
    jumpTime=blocks(8,(0-1))
    wheelSizer=blocks(9,(0-1))
    fallTime=blocks(10,(0-1))
    velX=blocks(11,(0-1))
    velY=blocks(12,(0-1))
    autoJump=blocks(13,(0-1))
    storedX=blocks(14,(0-1))
    storedY=blocks(15,(0-1))
    showNumb=280
    show="level restarted"
END IF
blockHit=0
downThree=0
If Multikey(SC_1)=(0-1) THEN downThree=1
If downThreeOld=0 and downThree=1 THEN
    blocks(5,(0-1))=X
    blocks(6,(0-1))=Y
    blocks(7,(0-1))=Z
    blocks(8,(0-1))=jumpTime
    blocks(9,(0-1))=wheelSizer
    blocks(10,(0-1))=fallTime
    blocks(11,(0-1))=velX
    blocks(12,(0-1))=velY
    blocks(13,(0-1))=autoJump
    blocks(14,(0-1))=storedX
    blocks(15,(0-1))=storedY
    showNumb=280
    show="saved player location"
END IF
downThreeOld=downThree
downSix=0
If Multikey(SC_6)=(0-1) THEN downSix=1
If downSixOld=0 and downSix=1 THEN
    If gamemode<>2 THEN gamemode=1
    If gamemode=2 THEN gamemode=0
    If gamemode=1 THEN gamemode=2
END IF
downSixOld=downSix
downSeven=0
If Multikey(SC_7)=(0-1) THEN downSeven=1
If downSevenOld=0 and downSeven=1 THEN
    If dynCam<>1 THEN dynCam=2
    If dynCam=1 THEN dynCam=0
    If dynCam=2 THEN dynCam=1
END IF
downSevenOld=downSeven
blocks(0,(0-1))=X
blocks(1,(0-1))=Y
If M=18 and dynCam=0 THEN
    blocks(0,selection)=mouseX
    blocks(1,selection)=mouseY
END IF
If M=18 and dynCam=0 THEN
    blocks(0,selection)=mouseX
    blocks(1,selection)=mouseY
END IF
If M=18 and dynCam=1 and selection=(0-1) THEN
    If mouseX<=resX*0.5-25 THEN blocks(0,selection)=blocks(0,selection)-2
    If mouseX>resX*0.5+25 THEN blocks(0,selection)=blocks(0,selection)+2
    If mouseY<=resY*0.5-25 THEN blocks(1,selection)=blocks(1,selection)-4
    If mouseY>resY*0.5+25 THEN blocks(1,selection)=blocks(1,selection)+2
END IF
If M=18 and dynCam=1 and selection<>(0-1) THEN
    If mouseX<=resX*0.5-25 THEN blocks(0,selection)=blocks(0,selection)-1
    If mouseX>resX*0.5+25 THEN blocks(0,selection)=blocks(0,selection)+1
    If mouseY<=resY*0.5-25 THEN blocks(1,selection)=blocks(1,selection)-1
    If mouseY>resY*0.5+25 THEN blocks(1,selection)=blocks(1,selection)+1
END IF
X=blocks(0,(0-1))
Y=blocks(1,(0-1))
bigger=0
blocks(2,(0-1))=Z
blocks(4,(0-1))=wheelSizer
If whSt>whStStored and blocks(4,selection)<20 THEN blocks(4,selection)=blocks(4,selection)+1
If whSt<whStStored and blocks(4,selection)>-20 THEN blocks(4,selection)=blocks(4,selection)-1
blocks(2,selection)=abs(blocks(4,selection))+5
whStStored=whSt
Z=blocks(2,0-1)
If selection=(0-1) and Z>ZStored THEN bigger=1
If selection=(0-1) THEN ZStored=Z
wheelSizer=blocks(4,(0-1))
If W="a" and F=0 THEN F=97
If W="d" and F=0 THEN F=100
If W="Esc" THEN F=27
jump()
If gamemode<>2 THEN
    If F=97 and bigger=0 THEN 
        IF W2=0 or W3=0 or W4=0 THEN X=X-1
    END IF
    If F=100 and bigger=0 THEN
        IF W1=0 or W2=0 or W3=0 THEN X=X+1
    END IF
    If F=115 THEN
        IF W1=0 or W2=0 or W3=0 THEN Y=Y +1
    END IF
END IF
If gamemode=2 THEN
    IF W1=0 or W2=0 or W3=0 THEN X=X+1
END IF
upY=1
k=0
items=0
cycleNumb=0
upTrue=0
While k<platforms
    X2=blocks(0,k)
    Y2=blocks(1,k)
    Z2=blocks(2,k)
    If X2-X<55 and X-X2<55 and Y2-Y<55 and Y-Y2<55 THEN
        items=items+1
        isAutoJump=blocks(5,k)
        collision(0,3)
    END IF
    k=k+1
WEND
If upTrue=1 THEN Y=newYPos
cycleNumb=1
gravity()
k=0
upTrue=0
While k<platforms
    X2=blocks(0,k)
    Y2=blocks(1,k)
    Z2=blocks(2,k)
    If X2-X<55 and X-X2<55 and Y2-Y<55 and Y-Y2<55 THEN
        items=items+1
        isAutoJump=blocks(5,k)
        collision(0,0)
    END IF
    k=k+1
WEND
If upTrue=1 THEN Y=newYPos
goalReachedOld=0
If goal=1 THEN goalReachedOld=1
goal=0
E=E+1
If F=27 GOTO ENDING
blocks(0,(0-1))=X
blocks(1,(0-1))=Y
velX=X-storedX
velY=Y-storedY
storedX=X
storedY=Y
If showNumb<300 THEN showNumb=showNumb+1
GOTO DISP



ENDING:
SYSTEM

fileLoad:
WHILE len(inkey)<>0
WEND
ChDir("C:\Users\" + username + "\SandboxPlatformer\levels\")
Cls
currentFilename=levelbrowser("Select SandboxPlatformer Level To Load...", "C:\Users\" + username + "\SandboxPlatformer\levels\")
If currentFilename<>"" THEN
    Open currentFilename FOR INPUT as #1
    errorCode=err
    If errorCode<>0 THEN
        showNumb=0
        show="error loading"
    END IF
    If errorCode=0 THEN
        Get #1,0,blocks()
        errorCode=err
        If errorCode<>0 THEN
            showNumb=0
            show="error loading"
        END IF
        If errorCode=0 THEN
            X=blocks(0,(0-1))
            Y=blocks(1,(0-1))
            Z=blocks(2,(0-1))
            fps=0
            sleeper=13
            frame=0
            jumpTime=88
            wheelSizer=0
            fallTime=0
            velX=0
            velY=0
            autoJump=0
            selection=(0-1)
            platforms=blocks(1,(0-2))
            gamemode=blocks(16,(0-1))
            dynCam=blocks(17,(0-1))
            blocks(5,(0-1))=X
            blocks(6,(0-1))=Y
            blocks(7,(0-1))=Z
            blocks(8,(0-1))=jumpTime
            blocks(9,(0-1))=wheelSizer
            blocks(10,(0-1))=fallTime
            blocks(11,(0-1))=velX
            blocks(12,(0-1))=velY
            blocks(13,(0-1))=autoJump
            blocks(14,(0-1))=X
            blocks(15,(0-1))=Y
            If blocks(0,(0-2))=00000003 THEN
                blocks(0,(0-2))=00000008
                showNumb=0
                show="level last saved in previous version 0.0.0.3"
            END IF
            If blocks(0,(0-2))=00000004 THEN
                blocks(0,(0-2))=00000008
                showNumb=0
                show="level last saved in previous version 0.0.0.4"
            END IF
            If blocks(0,(0-2))=00000005 THEN
                blocks(0,(0-2))=00000008
                showNumb=0
                show="level last saved in previous version 0.0.0.5"
            END IF
            If blocks(0,(0-2))=00000006 THEN
                blocks(0,(0-2))=00000008
                showNumb=0
                show="level last saved in previous version 0.0.0.6"
            END IF
            If blocks(0,(0-2))=00000007 THEN
                blocks(0,(0-2))=00000008
                showNumb=0
                show="level last saved in previous version 0.0.0.7"
            END IF
            If blocks(0,(0-2))>00000008 THEN
                blocks(0,(0-2))=00000008
                showNumb=0
                show="warning: level last saved in later version"
            END IF
        END IF
    END IF
    Close
END IF
GOTO loadReturn



fileSave:
WHILE len(inkey)<>0
WEND
ChDir("C:\Users\" + username + "\SandboxPlatformer\levels\")
Cls
currentFilename=levelbrowser("Select File To Save To", "C:\Users\" + username + "\SandboxPlatformer\levels\")
If currentFilename<>"" THEN
    Open currentFilename FOR OUTPUT as #1
    errorCode=err
    If errorCode<>0 THEN
        showNumb=0
        show="error saving"
    END IF
    If errorCode=0 THEN
        blocks((0-1),0)=X
        blocks((0-1),1)=Y
        blocks((0-1),2)=Z
        blocks(16,(0-1))=gamemode
        blocks(17,(0-1))=dynCam
        Put #1,0,blocks()
        errorCode=err
        If errorCode<>0 THEN
            showNumb=0
            show="error saving"
        END IF
    END IF
    Close
END IF
GOTO saveReturn

errorHandling:
Cls
Locate(1,1)
PRINT "THE GAME HAS ENCOUNTERED THE FOLLOWING ERROR"
PRINT errorCode
PRINT ""
PRINT ""
PRINT "PRESS BACKSPACE TO EXIT"
While k<>(0-1)
    k=Multikey(SC_BACKSPACE)
    SLEEP 50
WEND
SYSTEM