unit SizeControl;
{$IFDEF FPC}       {$MODE Delphi}       {$ENDIF}
(*
 ---------------------------------------------------------------------------
Component Name:  TSizeCtrl
Module:          SizeControl
Description:     Enables both moving and resizing of controls at runtime.
Version:         8.2
Date:            03-JUN-2019
Author:          Leu Zenin, kashaket@protonmail.com
                 Angus Johnson,   angusj@myrealbox.com
Copyright:      © 2019 Leu Zenin
                {near 94.45% code is refactored}
 --------------------------------------------------------------------------- *)

interface
{$R SIZECONTROL}
{$INCLUDE *.inc}
uses
  {$IFDEF FPC}LCLIntf, LCLType, LMessages,
  {$ELSE} Windows, Messages,{$ENDIF} SysUtils, Classes,
  Controls, Graphics,
  Dialogs,
  Menus,   //To hook the TSizeCtrl.PopupMenu
  ComCtrls, //To check the TTabSheet, TPageControl
 {$IFDEF VER3U} TypInfo, {$ENDIF} //To hook the OnClick event
 {$IFDEF VER3UP} Math, {$IFNDEF FPC}System.UITypes,{$ENDIF} {$ENDIF}
 //To calculate TSizeBtn shape region//TO expand the GetColor function for our needs
  Forms;
  (* [TSizeBtn reqs]
    To make transparent and topmost at the same time...
    Another way is to use TGraphicControl, but, it doesn't gives ability
    to use the Alpha-Blending :)
  *)

function getAbsoluteX(cntrl: TControl; LastControl: TControl): integer;
function getAbsoluteY(cntrl: TControl; LastControl: TControl): integer;
{$IFNDEF VER3UP}
function Floor(const X: Extended): Integer;
function Ceil(const X: Extended): Integer;
function min(const X, Y: Integer): Integer;
function max(const X, Y: Integer): Integer;
{$ENDIF}
{$IFDEF FPC}
const
  WM_SYSCOMMAND       = LM_SYSCOMMAND;
  WM_NCLBUTTONDBLCLK  = LM_NCLBUTTONDBLCLK;
  WM_NCLBUTTONDOWN    = LM_NCLBUTTONDOWN;
  WM_LBUTTONDOWN      = LM_LBUTTONDOWN;
  WM_NCLBUTTONUP      = LM_NCLBUTTONUP;
  WM_NCRBUTTONDOWN    = LM_CONTEXTMENU;
  WM_NCMOUSEMOVE      = LM_NCMOUSEMOVE;
  WM_LBUTTONUP        = LM_LBUTTONUP;
  WM_MOUSEMOVE        = LM_MOUSEMOVE;
  WM_MOUSEFIRST       = LM_MOUSEFIRST;
  WM_RBUTTONDOWN      = LM_RBUTTONDOWN;
  WM_MOUSELAST        = LM_MOUSELAST;
  WM_CREATE           = LM_CREATE;
  WM_DESTROY          = LM_DESTROY;
  WM_PARENTNOTIFY     = LM_PARENTNOTIFY;
  WM_SETCURSOR        = LM_SETCURSOR;
  WM_GETDLGCODE       = LM_GETDLGCODE;
  WM_KEYDOWN          = LM_KEYDOWN;
  WM_KEYUP            = LM_KEYUP;
  WM_CHAR             = LM_CHAR;
  WM_CANCELMODE       = LM_CANCELMODE;
type

          TMessage      = TLMessage;
          TWMMouse      = TLMMouse;
          TWMParentNotify      = TLMCommand;
          TWMSysCommand        = TLMSysCommand;
          TWMNCHitMessage      = TLMNCHITTEST;
  {$ELSE}
type
  {$ENDIF}
  TSizeCtrl = class;
  TTargetObj = class;
  TBtnPos = (bpNone, bpLeft, bpTop, bpRight, bpBottom, bpTopLeft, bpTopRight,
    bpBottomRight, bpBottomLeft);
  TBtnPosSet = set of TBtnPos;
  TSizeCtrlBtnCount = (szctrl4btns, szctrl8btns);
  TRecursionVector = (trecChild, trecParent, trecBoth);
  TSCState = (scsReady, scsMoving, scsSizing);

  TStartEndEvent = procedure(Sender: TObject; State: TSCState) of object;
  TDuringEvent = procedure(Sender: TObject; dx, dy: integer; State: TSCState) of object;
  TMouseDownEvent = procedure(Sender: TObject; Target: TControl;
    TargetPt: TPoint; var handled: boolean) of object;
  TSetCursorEvent = procedure(Sender: TObject; Target: TControl;
    TargetPt: TPoint; var handled: boolean) of object;

  TContextPopupEvent = procedure(Sender: TObject; MousePos: TPoint;
    var Handled: boolean) of object;
  TReSizeFrameType = (tszfNone, tszfButtons, tzfRButtons, tszfRButton);
  TReSizeHideType = (tszhNone, tszhMove, tszhHide);
  TSizeBtnShapeType = (tszbSquare, tszbTriangle, tszbCircle,
  tszbRoundRect, tszbRhombus, tszbMockTube);
  //TSizeBtn is used internally by TSizeCtrl.
  //There are 8 TSizeBtns for each target which are the target's resize handles.
  {$IFDEF FPC}
  TSizeBtn = class(TForm)
  {$ELSE}
  TSizeBtn = class(TCustomForm)
  {$ENDIF}
  private
    fTargetObj: TTargetObj;
    fPos: TBtnPos;
    {$IFDEF FPC}
    fPrevShape: TSizeBtnShapeType;
    fPrevSize: integer;
    {$ENDIF}
    fHoverDown, fHover: boolean;
    fLeft, fTop: integer;
    fColor, fPen: TColor;
    fImage: TPicture;
  protected
    function CalcTriangle(l, t:integer): TArray<TPoint>;
    procedure PaintAs(l,t:integer);
    procedure DrawAt(l,t:integer{$IFDEF FPC};b:Graphics.TBitmap{$ENDIF});
    procedure doPaint(Sender:TObject);
    procedure mEnter(Sender:TObject);
    procedure mLeave(Sender:TObject);
    procedure UpdateBtnCursorAndColor;
    {$IFDEF FPC}
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      {%H-}X, {%H-}Y: integer); override;
    {$ELSE}
     procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
    {$ENDIF}
    procedure dMouseUp;
    function GetTop: integer;
    function GetLeft: integer;
    procedure setLeft(value: integer);
    procedure SetTop(value: integer);
  public
    procedure Reset;
    property Left: integer read GetLeft write SetLeft;
    property Top: integer read GetTop write SetTop
    ;
    {$IFDEF FPC} {$PUSH} {$WARN 03057 OFF} {$ENDIF}
    constructor Create(TargetObj: TTargetObj; BtnPos: TBtnPos);
    {$IFDEF FPC} {$PUSH} {$ENDIF}
 {$IFNDEF VER3U} reintroduce; {$ENDIF}
  end;
  {$IFNDEF FPC}
  TMovePanel = class(TCustomForm)
  private
    fSizeCtrl: TSizeCtrl;
    procedure setfcanvas(fCanvas: TCanvas);
  protected
    procedure SetBoundsRect(Sender: TObject);
    procedure DoPaint(Sender: TObject);
  public
    constructor Create(AOwner: TComponent;AsizeCtrl:TSizeCtrl);
    {$IFNDEF VER3U} reintroduce; {$ENDIF}
  end;
  {$ENDIF}
  //TRegisteredObj is used internally by TSizeCtrl. Each TRegisteredObj
  //contains info about a possible target control.
  TRegisteredObj = class
  protected
    fSizeCtrl: TSizeCtrl; //the owner of TRegisteredObj
    fControl: TControl;
    fHooked: boolean;
    fOldWindowProc: TWndMethod;
    fOldClickMethod: TMethod;
    procedure Hook;
    procedure UnHook;
    procedure NewWindowProc(var Msg: TMessage);
    procedure NewWindowProcB(var Msg: TMessage);
  public
    constructor Create(aSizeCtrl: TSizeCtrl; aControl: TControl);
  {$IFNDEF VER3U} reintroduce; {$ENDIF}
    destructor Destroy; override;
  end;

  //TTargetObj is the container for each current target, and contains the 8
  //TSizeBtn objects. Any number of TTargetObj's can be contained by TSizeCtrl.
  TTargetObj = class
  private

    fSizeCtrl: TSizeCtrl; //the owner of TTargetObj
    fTarget: TControl;
    {$IFDEF FPC}
    p: TPen;
    br: TBRuSH;
    frw:TCustomControl;
    fDrawRect: boolean;
    {$ELSE}
    fPanels: TList;
    fPanelsNames: TStrings;
    {$ENDIF}
    fBtns: array [TBtnPos] of TSizeBtn;
    fFocusRect: TRect;
    fLastRect: TRect;
    fStartRec: TRect;
    procedure Update;
    procedure DragMoveUpdate(const Rectange: TRect;const t: byte);
    procedure StartFocus();
    function MoveFocus(dx, dy: integer): boolean;
    function SizeFocus(dx, dy: integer; BtnPos: TBtnPos): boolean;
    procedure EndFocus;
    procedure DrawRect(obj: TControl);
  public
    constructor Create(aSizeCtrl: TSizeCtrl; aTarget: TControl);
  {$IFNDEF VER3U} reintroduce; {$ENDIF}
    destructor Destroy; override;
    procedure ReconfButtons;
  end;
  TSizeCtrlTags = record
  private
    var
    fMove, fNMove,   //Move  --/ /-- fTopLeft
    fmMove, fnmMove,   //Multi-Move
    fResize, fnResize,  //Resize  --/ /-- fHeightWidth
    fMResize, fNMResize,  //Multi-Resize
     fChange, fnChange,    //Any change
     fMChange, fnMChange,   //Any change in selected group
     fNoSel, //Deny selection!
     { x; y; w; h; }
    fLeft, fTop, fWidth, fHeight,
      { x, h; x, w; }
    fLeftHeight, fLeftWidth,
      { y, h; y, w; }
    fTopHeight, fTopWidth,
      { x, y, w, h; x, y, w; x, y, h; }
    fTLWH, fTLW, fTLH: int64;
  public
    //ChangeTopLeft: integer read fnTopLeft write fnTopLeft;
    property DenySelect: int64 read fNoSel write fNoSel;
    property AllowMove: int64 read fMove write fMove;
    property DenyMove:  int64  read fNMove write fNMove;
    property AllowMultiMove:  int64  read fMMove write fMMove;
    property DenyMultiMove:  int64  read fNMMove write fNMMove;

    property AllowChange:  int64  read fChange write fChange;
    property DenyChange:  int64  read fNChange write fNChange;
    property AllowMultiChange:  int64  read fmChange write fmChange;
    property DenyMultiChange:  int64  read fnMChange write fnMChange;

    property ChangeTop:  int64  read fTop write fTop;
    property ChangeLeft:  int64  read fLeft write fLeft;
    property ChangeWidth:  int64  read fWidth write fWidth;
    property ChangeHeight:  int64  read fHeight write fHeight;

    property ChangeTopHeight:  int64  read fTopHeight write fTopHeight;
    property ChangeTopWidth:  int64  read fTopWidth write fTopWidth;
    property ChangeLeftHeight:  int64  read fLeftHeight write fLeftHeight;
    property ChangeLeftWidth:  int64  read fLeftWidth write fLeftWidth;

    property ChangeTopLeftWidth:  int64  read fTLW write fTLW;
    property ChangeTopLeftHeight:  int64  read fTLH write fTLH;
    property ChangeTopLeftWidthHeight:  int64  read fTLWH write fTLWH;

    // ChangeHeightWidth:  int64  read fnHeightWidth write fnHeightWidth;
    property AllowResize:   int64  read fResize write fResize;
    property DenyResize:  int64  read fNResize write fNResize;

    property AllowMultiResize:  int64  read fMResize write fMResize;
    property DenyMultiResize:  int64  read fNMResize write fNMresize;
  end;
  {$IFDEF FPC}
  TSizeCtrl = class(TForm)
  {$ELSE}
  TSizeCtrl = class(TComponent)
  {$ENDIF}
  private
    fLastBtn: TBtnPos;
    fTargetList: TList; //list of TTargetObj (current targets)
    fRegList: TList;    //list of TRegisteredObj (possible targets)
    fState: TSCState;
    fMoveOnly: boolean;
    fBtnAlpha, fBtnSize: integer;
    fClipRec: TRect;
    fStartPt: TPoint;
    fEnabledBtnColor: TColor;
    fHoverBtnColor: TColor;
    fDisabledBtnColor: TColor;
    fValidBtns: TBtnPosSet;
    fMultiResize: boolean;
    fEnabled, fAlignToGrid: boolean;
    fCapturedCtrl: TControl;
    fCapturedBtnPos: TBtnPos;
    fGridSize: integer;
    {$IFDEF FPC}
    fDefWindowProc,{$ENDIF}
    fOldWindowProc: TWndMethod;
    fEscCancelled: boolean;
    fParentForm: TCustomForm;
    {$IFNDEF FPC}
    fHandle: THandle;
    {$ENDIF}
    fPopupMenu: TPopupMenu;
    fOnContextPopup: TContextPopupEvent;
    fLMouseDownPending: boolean;
    fForm: TWinControl;
    fTags: TSizeCtrlTags;
    fGridWhite: TColor;
    fGridBlack: TColor;
    fBtnFrameColor: TColor;
    fHoverBtnFrameColor: TColor;
    fDisabledBtnFrameColor: TColor;
    fBtnShape: TSizeBtnShapeType;
    fReSizeType: TReSizeFrameType;
    fReIgnBtn: TReSizeHideType;
    fShowFrame, fApplySizes: boolean;
    fBtnCount: TSizeCtrlBtnCount;
    FSelKey,
      fSelActionSPKey,
      fSelToggleKey,
      fMoveLeftKey, fMoveTopKey,
      fMoveRightKey, fMoveBottomKey,
     FDGKey: integer;
    fStartEvent: TStartEndEvent;
    fDuringEvent: TDuringEvent;
    fEndEvent: TStartEndEvent;
    fTargetChangeEvent, fOnBh, fOnBtnUh: TNotifyEvent;
    fOnMouseDown: TMouseDownEvent;
    fOnMouseEnter: TMouseDownEvent;
    fOnSetCursor: TSetCursorEvent;
    fOnKeyDown: TKeyEvent;
    FShowGrid: boolean;
    fCanv: TCanvas;
    fMovePanelAlpha: integer;
    fPanelImage: TPicture;
    fBtnImage: TPicture;
    fHoverBtnImage: TPicture;
    fEditParent: boolean;
    fDisabledBtnImage: TPicture;
    fStretchBtnImage, fStretchPanelImage: boolean;
    function GetTargets(index: integer): TControl;
    function GetTargetCount: integer
    ;
    {$IFDEF FPC} {$PUSH} {$WARN 03057 OFF} {$ENDIF}
    procedure SetEnabled(Value: boolean);
    {$IFDEF FPC} {$PUSH} {$ENDIF}
    procedure WinProc(var iMsg: TMessage);
    procedure FormWindowProc(var Msg: TMessage);
    procedure DoWindowProc(DefaultProc: TWndMethod; var Msg: TMessage);

    procedure DrawRect;
    procedure SetMoveOnly(Value: boolean);
    function IsValidSizeBtn(BtnPos: TBtnPos): boolean;
    function IsValidMove: boolean;
    procedure SetMultiResize(Value: boolean);
    procedure SetPopupMenu(Value: TPopupMenu);
    procedure DoPopupMenuStuff;
    procedure setGridSize(Value: integer);
    procedure setGridWhite(Value: TColor);
    procedure setGridBlack(Value: TColor);
    procedure SetBtnCount(Value: TSizeCtrlBtnCount);
    procedure setBtnSize(Value: integer);
    procedure setBtnAlphaBlend(Value: integer);
    procedure setMovePanelAlphaBlend(Value: integer);
    procedure setPanelImage(Value: TPicture);
    procedure setBtnImage(Value: TPicture);
    procedure setHoverBtnImage(Value: TPicture);
    procedure setDisabledBtnImage(Value: TPicture);
    procedure setStretchBtnImage(Value: boolean);
    procedure SetEnabledBtnColor(aColor: TColor);
    procedure SetHoverBtnColor(aColor: TColor);
    procedure SetDisabledBtnColor(aColor: TColor);
    procedure SetBtnShape(v: TSizeBtnShapeType);
    procedure SetBtnFrameColor(aColor: TColor);
    procedure SetHoverBtnFrameColor(aColor: TColor);
    procedure SetDisabledBtnFrameColor(aColor: TColor);
    {$IFDEF FPC}
    procedure DoMouseDown(Sender: TObject; {%H-}Button: TMouseButton; Shift: TShiftState);
    procedure DoMouseUp(Sender: TObject; {%H-}Button: TMouseButton; {%H-}Shift: TShiftState);
    procedure DoMouseMove(Sender: TObject; {%H-}Shift: TShiftState);
    {$ELSE}
    procedure DoMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState);
    procedure DoMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState);
    procedure DoMouseMove(Sender: TObject; Shift: TShiftState);
    {$ENDIF}
    procedure SetShowGrid(const Value: boolean);
  protected
    {$IFNDEF FPC}
    fGrid: TBitmap; // сетка
    {$ENDIF}
    fGridForm: TForm;
    lastW: integer;
    lastH: integer; // последнЯЯ ширина и высота формы
    lastColor: TColor; // последний цвет формы
    FConstraints: TSizeConstraints;
    procedure CreateGrid;
    procedure Hide;
    procedure Show;
    procedure UpdateGrid;
    procedure HardReset(sizes:boolean=false);
    procedure UpdateBtnCursors;
    procedure MoveTargets(dx, dy: integer);
    procedure SizeTargets(dx, dy: integer);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    {$IFDEF FPC}
    function DoKeyDown(var Message: TLMKey): boolean;
    {$ELSE}
    function DoKeyDown(var Message: TWMKey): boolean;
    {$ENDIF}
    procedure formPaint(Sender: TObject);
    function PerformRecursively
    (
      Control: TControl;
      Func: byte;
      FullRecursion: boolean;
      Include:boolean;
      AddType:TRecursionVector
    ): boolean;
    function XYweckPositive(x,y:integer;c:byte;t:int64): boolean;
    function XyWeckTag(c: TBtnPos;t:int64):boolean;
    function XYweck(x,y:integer;c:byte;t:int64): boolean;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function isTarget(tcrl: TControl): boolean;
    function FixSize(input, wh: integer): integer;
    function KeysToShiftState(Keys: Word): TShiftState;
    function KeyDataToShiftState(): TShiftState;
    property LastBtn: TBtnPos read fLastBtn;
    property TagOptions: TSizeCtrlTags read fTags write fTags;
    //Targets: used to access individual targets (read-only)
    property Targets[index: integer]: TControl read GetTargets;
  published
  function RegisteredCtrlFromPt(screenPt: TPoint;
      ParentX: TWinControl = nil): TControl;
    //Update: it is the responsibility of the component user to call Update
    //if the target(s) are moved or resized independently of this control
    //(eg if the form is resized and targets are aligned with it.)
    procedure Update; {$IFDEF FPC}override;{$ENDIF}
    procedure UpdateBtns;

    procedure toFront(CNTR: TControl);
    procedure toBack(CNTR: TControl);

    //RegisterControl: Register potential target controls with TSizeCtrl
    function RegisterControl(Control: TControl): integer;
    function RegisterControlR
    (
      Control: TControl;
      AddType: TRecursionVector=TRecursionVector.trecChild;
      FullRecursion: boolean=True
    ): boolean;
    function UnRegisterControl(Control: TControl): integer;
    function UnRegisterControlR
    (
      Control: TControl;
      AddType: TRecursionVector=TRecursionVector.trecChild;
      FullRecursion: boolean=True
    ): boolean;
    procedure UnRegisterAll;
    function RegisteredIndex(Control: TControl): integer;

    function getRegObj(C: TComponent): TRegisteredObj;
    function getSelected(): TList;

    //AddTarget: Add any number of targets to TSizeCtrl so they can be
    //resized or moved together.
    //(nb: The programmer doesn't normally need to call this method directly
    //since TSizeCtrl will call it whenever a target is clicked.)
    function AddTarget(Control: TControl): integer;
    function AddTargetR
    (
      Control: TControl;
      AddType: TRecursionVector=TRecursionVector.trecChild;
      FullRecursion: boolean=True
    ): boolean;
    function DeleteTarget(Control: TControl): integer;
    function DeleteTargetR
    (
      Control: TControl;
      AddType: TRecursionVector=TRecursionVector.trecChild;
      FullRecursion: boolean=True
    ): boolean;
    procedure ClearTargets;
    function TargetIndex(Control: TControl): integer;
    function TargetCtrlFromPt(screenPt: TPoint): TControl;

    //Enabled: This key property should be self-explanatory.
    property Enabled: boolean read fEnabled write SetEnabled;
    //<summary>
    //Used for getting targets count
    //</summary>
    property TargetCount: integer read GetTargetCount;
    property EditParent: boolean read fEditParent write fEditParent;
    property Parent:TWinControl read FForm;
    //MinWidth: minimal target (resizing) width
    //MinHeight: minimal target (resizing) height
    //MaxWidth: maximal target (resizing) width
    //MaxHeight: maximal target (resizing) height
    property Constraints: TSizeConstraints read FConstraints write FConstraints;
    //MoveOnly: ie prevents resizing
    property MoveOnly: boolean read fMoveOnly write SetMoveOnly;
    //BtnCount: Count of the grab buttons
    property BtnCount: TSizeCtrlBtnCount read fBtnCount write setBtnCount;
    //BtnSize: Size of a grab-handle buttons
    property BtnSize: integer read FBtnSize write setBtnSize;
    //BtnAlphaBlend: Alpha-blend semitransparent multiplier for grab btns
    property BtnAlphaBlend: integer read fBtnAlpha write setBtnAlphaBlend;
    //BtnColor: Color of grab-handle buttons
    property BtnColor: TColor read fEnabledBtnColor write SetEnabledBtnColor;
    //BtnColor: Color of grab-handle buttons
    property HoverBtnColor: TColor read fHoverBtnColor write SetHoverBtnColor;

    //BtnColorDisabled: eg grab buttons along aligned edges of target controls
    property DisabledBtnColor: TColor read fDisabledBtnColor write SetDisabledBtnColor;
    property BtnShape: TSizeBtnShapeType read fBtnShape write setBtnShape;
    property BtnFrameColor: TColor read fBtnFrameColor write setBtnFrameColor;
    property HoverBtnFrameColor: TColor read fHoverBtnFrameColor write setHoverBtnFrameColor;
    property DisabledBtnFrameColor: TColor read fDisabledBtnFrameColor write setDisabledBtnFrameColor;
    //BtnImage eg grab buttons along 8 edges of target controls
    property BtnImage: TPicture read fBtnImage write setBtnImage;
    //Hover-state button(s)
    property HoverBtnImage: TPicture read fHoverBtnImage write setHoverBtnImage;
    //DisabledBtnImage - you will understand
    property DisabledBtnImage: TPicture read fDisabledBtnImage write setDisabledBtnImage;

    //BtnImage eg grab buttons along 8 edges of target controls
    property StretchBtnImage: Boolean read fStretchBtnImage write setStretchBtnImage;

    //Custom keymaps
    property SelectionKey: integer read FSelKey write FSelKey;
    property SelectionSelectParentKey:
      integer read fSelActionSPKey write fSelActionSPKey;
    property SelectionTabKey:
      integer read  fSelToggleKey write fSelToggleKey;
    property MoveLeftKey: integer read fMoveLeftKey write fMoveLeftKey;
    property MoveRightKey:integer read fMoveRightKey write fMoveRightKey;
    property MoveUpKey:integer read  fMoveTopKey write fMoveTopKey;
    property MoveDownKey:integer read fMoveBottomKey write fMoveBottomKey;
    property DisableGridAlignKey: integer read FDGKey write FDGKey;

    property ShowGrid: boolean read FShowGrid write SetShowGrid;
    //GridSize: aligns mouse moved/resized controls to nearest grid dimensions
    property GridSize: integer read fGridSize write setGridSize;
    property AlignToGrid: boolean read fAlignToGrid write fAlignToGrid;
    property GridColor: TColor read fGridBlack write setGridBlack;
    property GridColorContrast: TColor read fGridWhite write setGridWhite;
    //MultiTargetResize: Resizing of multiple targets is allowed by default
    //as long as this isn't impeded by specific Target control alignments
    property MultiTargetResize: boolean read fMultiResize write SetMultiResize;
    //The TMovePanel canvases (applicable to all descedants)
    property MovePanelCanvas: TCanvas read fCanv;
    property MovePanelImage: TPicture read fPanelImage write setPanelImage;
    property StretchMovePanelImage: boolean read fStretchPanelImage write fStretchPanelImage;
    //The TMovePanel alphachannel (applicable to all descedants)
    property MovePanelAlphaBlend: Integer read fMovePanelAlpha write setMovePanelAlphaBlend;
    //Apply sizes when moving
    property ApplySizes: boolean read fApplySizes write fApplySizes default false;
    //Show selection frame
    property ShowFrame: boolean read fShowFrame write fShowFrame default true;
    //Resize showing type
    property ResizeFrameType: TReSizeFrameType read fReSizeType write fReSizeType;
    //Ignoring method for unwanted btns
    property ResizeIgnoreMethod: TReSizeHideType read fReIgnBtn write fReIgnBtn;
    property PopupMenu: TPopupMenu read fPopupMenu write SetPopupMenu;
    //Self-explanatory Events ...
    property OnStartSizeMove: TStartEndEvent read fStartEvent write fStartEvent;
    property OnDuringSizeMove: TDuringEvent read fDuringEvent write fDuringEvent;
    property OnEndSizeMove: TStartEndEvent read fEndEvent write fEndEvent;
    property OnTargetChange: TNotifyEvent read fTargetChangeEvent
      write fTargetChangeEvent;
    property onButtonHover: TNotifyEvent read fOnBH write fOnBH;
    property onButtonUnhover: TNotifyEvent read fOnBtnUh write fOnBtnUh;
    property OnKeyDown: TKeyEvent read fOnKeyDown write fOnKeyDown;
    property OnMouseDown: TMouseDownEvent read fOnMouseDown write fOnMouseDown;
    property OnMouseEnter: TMouseDownEvent read fOnMouseEnter write fOnMouseEnter;
    property OnSetCursor: TSetCursorEvent read fOnSetCursor write fOnSetCursor;
    property OnContextPopup: TContextPopupEvent
      read fOnContextPopup write fOnContextPopup;
  end;

const
  CM_LMOUSEDOWN = WM_USER + $1;
  CM_RMOUSEDOWN = WM_USER + $2;

procedure Register;

implementation

uses Types;

type
  THackedControl = class(TControl){$IFNDEF FPC};{$ELSE}
  public
  procedure SendCancelMode(Sender: TControl);
  end;
  {$ENDIF}
  THackedWinControl = {$IFNDEF FPC}class(TWinControl);{$ELSE}
  class(TCustomForm)
  public
  procedure SendCancelMode(Sender: TControl);
  end;
  {$ENDIF}
procedure Register;
begin
  RegisterComponents('Samples', [TSizeCtrl]);
end;

{$IFDEF VER3D} type
  TAlignSet = set of TAlign; {$ENDIF}


//turn warnings off concerning unsafe typecasts since we know they're safe...
{$WARNINGS OFF}


//------------------------------------------------------------------------------
// Miscellaneous functions
//------------------------------------------------------------------------------
{$IFNDEF VER3UP}
function Floor(const X: Extended): integer;
begin
  Result := Integer(Trunc(X));
  if Frac(X) < 0 then
    Dec(Result);
end;
function Ceil(const X: Extended): Integer;
begin
  Result := Integer(Trunc(X));
  if Frac(X) > 0 then
    Inc(Result);
end;
function Min(const X, Y: Integer): Integer;
begin
  if X < Y then
    Result := X
  else
    Result := Y;
end;
function Max(const X, Y: Integer): Integer;
begin
  if X > Y then
    Result := X
  else
    Result := Y;
end;
{$ENDIF}
{$IFDEF FPC}
procedure THackedControl.SendCancelMode(Sender: TControl);
  procedure SendWMMode(F:TCustomForm;Sender: TControl);
  begin

      F.ActiveControl.Perform(CM_CANCELMODE, 0, LPARAM(Sender));
    if (F.FormStyle = fsMDIForm) and (F.ActiveMDIChild <> nil) then
      SendWMMode(F.ActiveMDIChild, Sender);
  end;
var
  Control: TControl;
begin
  Control := Self;
  while Control <> nil do
  begin
    if Control is TCustomForm then
     SendWMMode( TCustomForm(Control), Sender);
    Control := Control.Parent;
  end;
end;

procedure THackedWinControl.SendCancelMode(Sender: TControl);
  procedure SendWMMode(F:TCustomForm;Sender: TControl);
  begin

      F.ActiveControl.Perform(CM_CANCELMODE, 0, LPARAM(Sender));
    if (F.FormStyle = fsMDIForm) and (F.ActiveMDIChild <> nil) then
      SendWMMode(F.ActiveMDIChild, Sender);
  end;
var
  Control: TControl;
begin
  Control := Self;
  while Control <> nil do
  begin
    if Control is TCustomForm then
     SendWMMode( TCustomForm(Control), Sender);
    Control := Control.Parent;
  end;
end;
{$ENDIF}
function getAbsoluteX(cntrl: TControl; LastControl: TControl): integer;
begin
  Result := cntrl.Left;

  if integer(cntrl.Parent) <> integer(LastControl) then
    Result := Result + getAbsoluteX(cntrl.Parent, LastControl);
end;

function getAbsoluteY(cntrl: TControl; LastControl: TControl): integer;
begin
  Result := cntrl.top;

  if integer(cntrl.Parent) <> integer(LastControl) then
    Result := Result + getAbsoluteY(cntrl.Parent, LastControl);
end;

//------------------------------------------------------------------------------

function IsVisible(Control: TControl): boolean;
begin
  Result := True;
  while assigned(Control) do
    if Control is TCustomForm then
      exit
    else if not Control.Visible then
      break
    else
      Control := Control.Parent;
  Result := False;
end;

//------------------------------------------------------------------------------

function GetBoundsAsScreenRect(Control: TControl): TRect;
begin
  //GetBoundsAsScreenRect() assumes 'Control' is both assigned and has a parent.
  //Not all TControls have handles (ie only TWinControls) so ...
  with Control do
  begin
    Result.TopLeft := parent.ClientToScreen(BoundsRect.TopLeft);
    Result.Right := Result.Left + Width;
    Result.Bottom := Result.Top + Height;
  end;
end;

//------------------------------------------------------------------------------

function PointIsInControl(screenPt: TPoint; Control: TControl): boolean;
begin
  //PointIsInControl() assumes 'Control' is both assigned and has a parent.
  Result := PtInRect(GetBoundsAsScreenRect(Control), screenPt);
end;

//------------------------------------------------------------------------------

function KeyIsPressed(key: integer): boolean;
begin
  if key = -1 then
    Result := true
  else
   if key = 0 then
     Result := false
    Else
     Result := GetKeyState(key) < 0;
end;

//------------------------------------------------------------------------------
{
function inSizeTag(key: integer;arr: array of integer): boolean;
var i: integer;
begin
  Result := False;
  for i in arr do
    if i = key then
    begin
      Result := true;
      Exit;
    end;
end;
 //P.s Delphi compiler is kind of a shit
}
//------------------------------------------------------------------------------

function TSizeCtrl.XyWeck(x,y:integer;c:byte;t:int64): boolean;
begin
  Result :=
    (
     (fTargetList.Count > 1) and
     (
      (fTags.DenyMultiChange = t) or
      ((c=1)and(fTags.DenyMultiResize = t)) or
      ((c=0)and(fTags.DenyMultiMove = t))
     )
    )
    or
    (fTags.DenyChange = t)
    or
    (fTags.DenySelect = t)//Should never happen,but...
    or
    (
    (c = 0)
       and
        (fTags.DenyMove = t)
        or
         ((x<>0)
              and
               (
                 (fTags.ChangeTop=t)
                 or
                 (fTags.ChangeTopHeight=t)
                 or
                 (fTags.ChangeTopWidth=t)
                 or
                 (fTags.ChangeWidth=t)
                 or
                 (fTags.ChangeHeight=t)
               )
         )
        or
         ((y<>0)
              and
               (
                 (FTags.ChangeWidth=t)
                 or
                 (fTags.ChangeHeight=t)
                 or
                 (fTags.ChangeLeftHeight=t)
                 or
                 (fTags.ChangeLeft=t)
                 or
                 (fTags.ChangeLeftWidth=t)
               )
         )
    )
    or
    (
    (c = 1)
       and
        (fTags.DenyResize = t)
        or
         ((x<>0)
              and
               (
                 (fTags.ChangeTop=t)
                 or
                 (fTags.ChangeTopHeight=t)
                 or
                 (fTags.ChangeLeft=t)
                 or
                 (fTags.ChangeLeftHeight=t)
                 or
                 (fTags.ChangeHeight=t)
               )
         )
        or
         ((y<>0)
              and
               (
                 (FTags.ChangeWidth=t)
                 or
                 (fTags.ChangeTopWidth=1)
                 or
                 (fTags.ChangeTopHeight=t)
                 or
                 (fTags.ChangeLeft=t)
                 or
                 (fTags.ChangeLeftWidth=t)
               )
         )
    )
    ;
end;

//------------------------------------------------------------------------------

function TSizeCtrl.XyWeckPositive(x,y:integer;c:byte;t:int64): boolean;
begin
  Result :=
    (
     (fTargetList.Count > 1) and
     (
      (fTags.AllowMultiChange = t) or
      ((c=1)and(fTags.AllowMultiResize = t)) or
      ((c=0)and(fTags.AllowMultiMove = t))
     )
    )
    or
    (fTags.AllowChange = t)
    or
    (
    (c = 0)
       and
        (fTags.AllowMove = t)
        or
         ((x<>0)
              and
               not (
                 (fTags.ChangeTop=t)
                 or
                 (fTags.ChangeTopHeight=t)
                 or
                 (fTags.ChangeTopWidth=t)
                 or
                 (fTags.ChangeWidth=t)
                 or
                 (fTags.ChangeHeight=t)
               )
         )
        or
         ((y<>0)
              and
               not (
                 (FTags.ChangeWidth=t)
                 or
                 (fTags.ChangeHeight=t)
                 or
                 (fTags.ChangeLeftHeight=t)
                 or
                 (fTags.ChangeLeft=t)
                 or
                 (fTags.ChangeLeftWidth=t)
               )
         )
    )
    or
    (
    (c = 1)
       and
        (fTags.AllowResize = t)
        or
         ((x<>0)
              and
               not (
                 (fTags.ChangeTop=t)
                 or
                 (fTags.ChangeTopHeight=t)
                 or
                 (fTags.ChangeLeft=t)
                 or
                 (fTags.ChangeLeftHeight=t)
                 or
                 (fTags.ChangeHeight=t)
               )
         )
        or
         ((y<>0)
              and
               not (
                 (FTags.ChangeWidth=t)
                 or
                 (fTags.ChangeTopWidth=1)
                 or
                 (fTags.ChangeTopHeight=t)
                 or
                 (fTags.ChangeLeft=t)
                 or
                 (fTags.ChangeLeftWidth=t)
               )
          )
    )
    ;
end;

//-----------------------------------------------------------------------------
function TSizeCtrl.XyWeckTag(c: TBtnPos;t:int64):boolean;
begin

  case c of
    bpLeft:
      Result :=
      XyWeckPositive(1,0,0,t) and XyWeckPositive(1,0,1,t);
    bpTop:
      Result :=
      XyWeckPositive(0,1,0,t) and XyWeckPositive(0,1,1,t);
    bpRight:
      Result :=
      XyWeckPositive(1,0,1,t);
    bpBottom:
      Result :=
      XyWeckPositive(0,1,1,t);
    bpTopLeft:
      Result :=
      XyWeckPositive(1,0,0,t) and XyWeckPositive(1,0,1,t) AND XyWeckPositive(0,1,0,t) and XyWeckPositive(0,1,1,t);
    bpTopRight:
      Result :=
      XyWeckPositive(1,0,1,t) AND XyWeckPositive(0,1,0,t) and XyWeckPositive(0,1,1,t);
    bpBottomRight:
      Result :=
      XyWeckPositive(1,0,1,t) AND XyWeckPositive(0,1,1,t);
    bpBottomLeft:
      Result :=
      XyWeckPositive(1,0,0,t) and XyWeckPositive(1,0,1,t) AND XyWeckPositive(0,1,1,t);
  end;
end;

//------------------------------------------------------------------------------

function checkTag(tag: integer; _pi: TBtnPos; ts: TSizeCtrlTags;tCount:integer):boolean;
begin
If Tag = 0 then
  Result := False
Else
Result :=
    (Tag = ts.DenyChange)
    or
    (Tag = ts.DenyResize)
    or
    ((Tag = ts.DenyMultiResize) and (tCount > 1))
    or
    ((Tag = ts.DenyMultiChange) and (tCount > 1))
    or
    ((Tag = ts.ChangeTop) and (_pi <> bpTop))
    or
    ((Tag = ts.ChangeLeft) and (_pi <> bpLeft))
    or
    ((Tag = ts.ChangeHeight) and (_pi <> bpBottom))
    or
    ((Tag = ts.ChangeWidth) and (_pi <> bpRight))
    or
    ((Tag = ts.ChangeTopHeight) and
      (_pi <> bpTop) and (_pi <> bpBottom))
    or
    ((Tag = ts.ChangeTopWidth) and
      (_pi <> bpTop) and (_pi <> bpRight))
    or
    ((Tag = ts.ChangeLeftHeight) and
      (_pi <> bpLeft) and (_pi <> bpBottom))
    or
    ((Tag = ts.ChangeLeftWidth) and
      (_pi <> bpLeft) and (_pi <> bpRight))
    or
    ((Tag = ts.ChangeTopLeftWidth) and
      (_pi <> bpTop) and (_pi <> bpLeft) and (_pi<>bpRight))
    or
    ((Tag = ts.ChangeTopLeftHeight) and
      (_pi <> bpTop) and (_pi <> bpLeft) and (_pi<>bpBottom))
    or
    ((Tag = ts.ChangeTopLeftWidthHeight) and
       (_pi <> bpTop) and (_pi <> bpLeft)
       and
       (_pi <> bpBottom) and (_pi <> bpRight));
end;

//------------------------------------------------------------------------------

procedure AlignCtrlToGrid(Ctrl: TControl; ProposedBoundsRect: TRect);
begin
  with ProposedBoundsRect do
    Ctrl.SetBounds(left, top, right, bottom);
end;

//------------------------------------------------------------------------------
// TRegisteredObj functions
//------------------------------------------------------------------------------
{ TregisteredObj }

constructor TRegisteredObj.Create(aSizeCtrl: TSizeCtrl; aControl: TControl);
begin
  inherited Create;
  fSizeCtrl := aSizeCtrl;
  fControl := aControl;
    if fSizeCtrl.Enabled then
      Hook;
end;

//------------------------------------------------------------------------------

destructor TRegisteredObj.Destroy;
begin
  UnHook;
  inherited Destroy;
end;

//------------------------------------------------------------------------------

procedure TRegisteredObj.Hook;
var
  meth: TMethod;
begin
  if fHooked then
    exit;

  if fControl is TTabSheet then
    exit;

  fOldWindowProc := fControl.WindowProc;

  if fControl is TCustomForm then
  begin
    fControl.WindowProc := NewWindowProcB;
  end
  else
  fControl.WindowProc := NewWindowProc;
  //The following is needed to block OnClick events when TSizeCtrl is enabled.
  //(If compiling with Delphi 3, you'll need to block OnClick events manually.)
  {$IFDEF VER3U}

  if IsPublishedProp(fControl, 'OnClick') then
  begin
    meth := GetMethodProp(fControl, 'OnClick');
    fOldClickMethod.Code := meth.Code;
    fOldClickMethod.Data := meth.Data;

    meth.Code := nil;
    meth.Data := nil;
    SetMethodProp(fControl, 'OnClick', meth);
  end;
  {$ENDIF}

  fHooked := True;
end;

//------------------------------------------------------------------------------

procedure TRegisteredObj.UnHook;
var
  meth: TMethod;
begin
  if not fHooked then
    exit;
  if fControl is TTabSheet then
    exit;

  fControl.WindowProc := fOldWindowProc;

  {$IFDEF VER3U}
  try
    if IsPublishedProp(fControl, 'OnClick') then
    begin
      meth.Code := fOldClickMethod.Code;
      meth.Data := fOldClickMethod.Data;
      SetMethodProp(fControl, 'OnClick', meth);
    end;
  except
  end;
  {$ENDIF}

  fHooked := False;
end;

//------------------------------------------------------------------------------

procedure TRegisteredObj.NewWindowProc(var Msg: TMessage);
begin
  fSizeCtrl.DoWindowProc(fOldWindowProc, Msg);
end;

//------------------------------------------------------------------------------

procedure TRegisteredObj.NewWindowProcB(var Msg: TMessage);
begin
  CASE Msg.Msg of
  WM_SYSCOMMAND:
  case TWMSysCommand(Msg).CmdType of
   SC_SIZE, SC_MOVE, SC_MINIMIZE, SC_MAXIMIZE, SC_CLOSE,
   SC_MOUSEMENU, SC_KEYMENU, SC_RESTORE: TWMSysCommand(Msg).CmdType := SC_DEFAULT;
  END;
  {$IFDEF FPC} LM_NCHITTEST,{$ENDIF}
  WM_NCLBUTTONDBLCLK, WM_NCLBUTTONDOWN:
  begin
    {$IFDEF FPC}
    if Msg.Result <> HTCLIENT then
       Msg.Result := HTCLIENT;
    {$ELSE}
    if TWMNCHitMessage(Msg).HitTest <> HTCLIENT then
      TWMNCHitMessage(Msg).HitTest := HTCLIENT;
    {$ENDIF}
  end;
  WM_NCLBUTTONUP: Msg.Msg := WM_LBUTTONUP;
  WM_NCRBUTTONDOWN: Msg.Msg := WM_RBUTTONDOWN;
  WM_NCMOUSEMOVE: Msg.Msg := WM_MOUSEMOVE;
  END;
  fSizeCtrl.DoWindowProc(fOldWindowProc, Msg);
end;

//------------------------------------------------------------------------------
// TSizeBtn methods
//------------------------------------------------------------------------------

constructor TSizeBtn.Create(TargetObj: TTargetObj; BtnPos: TBtnPos);
begin
  inherited CreateNew(nil);
  Loaded;
  fTargetObj := TargetObj;
  AutoSize := False;
  Visible := False;
  Position := poDesigned;
  FormStyle := fsStayOnTop;
  Width := fTargetObj.fSizeCtrl.BtnSize;
  Height := fTargetObj.fSizeCtrl.BtnSize;
  {$IFNDEF FPC}
  {$IFDEF VER3UP}
  Color :=
  Floor(
  (fTargetObj.fSizeCtrl.BtnColor + fTargetObj.fSizeCtrl.DisabledBtnColor
  + integer(fTargetObj.fSizeCtrl.BtnShape) + fTargetObj.fSizeCtrl.BtnFrameColor)
  /4) + 1;
  TransparentColorValue := Color;
  TransparentColor := true;
  {$ELSE}
  Color := fTargetObj.fSizeCtrl.fParentForm.Color;
  {$ENDIF}
  {$ENDIF}
  {$IFNDEF GTK}
  BorderIcons := [];
  BorderStyle := bsNone;
  {$ENDIF}
  fHover := false;
  fHoverDown := false;
  OnPaint := doPaint;
  OnMouseEnter := mEnter;
  OnMouseLeave := mLeave;
  fPos := BtnPos;
  {$IFDEF FPC}
  fPrevShape   := TSizeBtnShapeType.tszbCircle;
  fPrevSize    := -1;
  {$ENDIF}
  UpdateBtnCursorAndColor;
end;

//------------------------------------------------------------------------------

procedure TSizeBtn.UpdateBtnCursorAndColor;
begin
  if not (fPos in fTargetObj.fSizeCtrl.fValidBtns) or
    (
     fTargetObj.fSizeCtrl.fMoveOnly and Not
      fTargetObj.fSizeCtrl.XyWeckTag(fPos,fTargetObj.fTarget.Tag)
    )
    or
    checkTag(fTargetObj.fTarget.Tag, fPos, fTargetObj.fSizeCtrl.TagOptions,
    fTargetObj.fSizeCtrl.TargetCount)
     then
  begin
    Cursor := crDefault;
    fColor := fTargetObj.fSizeCtrl.DisabledBtnColor;
    fPen := fTargetObj.fSizeCtrl.DisabledBtnFrameColor;
    fImage := fTargetObj.fSizeCtrl.DisabledBtnImage;
  end
  else
  begin
    case fPos of
      bpLeft, bpRight: Cursor := crSizeWE;
      bpTop, bpBottom: Cursor := crSizeNS;
      bpTopLeft, bpBottomRight: Cursor := crSizeNWSE;
      bpTopRight, bpBottomLeft: Cursor := crSizeNESW;
    end;
    if (fHover) or (fHoverDown) then
    Begin
      fColor := fTargetObj.fSizeCtrl.HoverBtnColor;
      fPen := fTargetObj.fSizeCtrl.HoverBtnFrameColor;
      fImage := fTargetObj.fSizeCtrl.HoverBtnImage;
    END
    ELSE
    Begin
      fColor := fTargetObj.fSizeCtrl.BtnColor;
      fPen := fTargetObj.fSizeCtrl.BtnFrameColor;
      fImage := fTargetObj.fSizeCtrl.BtnImage;
    End;
  end;
  if ParentWindow <> 0 then
  Repaint;
end;

//------------------------------------------------------------------------------

function TSizeBtn.GetTop; //1/2 Top-position flickering fix
                        //in delphi XE8 developers removed
                        //duplicate value checking from VCL...
begin
  Result := inherited Top;
end;

//------------------------------------------------------------------------------

function TSizeBtn.GetLeft;
begin
  fLeft := inherited Left;
  Result := fLeft;
end;

//------------------------------------------------------------------------------

procedure TSizeBtn.SetLeft(value: integer);
begin
  if fLeft <> value then
  begin
    inherited Left := value;
    fLeft := value;
  end;
end;

//------------------------------------------------------------------------------

procedure TSizeBtn.SetTop(value: integer);//2/2 Top-position flickering fix
begin
  if fTop <> value then
  begin
    inherited Top := value;
    fTop := value;
  end;
end;

//------------------------------------------------------------------------------

procedure TSizeBtn.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  if Button = mbLeft then
  begin
    fHoverDown := True;
    fTargetObj.fSizeCtrl.DoMouseDown(self, Button, Shift);
  end;
end;

//------------------------------------------------------------------------------

procedure TSizeBtn.dMouseUp;
begin
  fHover := False;
  fHoverDown := False;
  if Assigned( fTargetObj.fSizeCtrl.onButtonUnhover) then
    fTargetObj.fSizeCtrl.onButtonUnhover(Self as TObject);
  fTargetObj.fSizeCtrl.fCapturedBtnPos := bpNone;
  UpdateBtnCursorAndColor;
end;

//------------------------------------------------------------------------------

function TSizeBtn.CalcTriangle(l,t:integer): TArray<TPoint>;
begin
  case fPos of
    bpLeft:
    Result :=
      [Point(l, t+Floor(Height/2)),
      Point(l+Width-1, t),
      Point(l+Width-1, t+Height)
      ];
    bpRight:
    Result :=
      [Point(l+Width, t+Floor(Height/2)),
      Point(l, t),
      Point(l, t+Height-1)
      ];
    bpTop:
    Result :=
      [Point(l, t+Height-1),
      Point(l+Floor(Width/2), t),
      Point(l+Width, t+Height-1)
      ];
    bpTopLeft:
    Result :=
      [Point(l, t),
       Point(l+Width-1, t+Floor(Height/2)),
       Point(l+Floor(Width/2),t+Height-1)
      ];
    bpTopRight:
    Result :=
      [Point(l, t+Floor(Height/2)),
       Point(l+Floor(Width/2),t+Height-1),
       Point(l+Width, t-1)
      ];
    bpBottom:
    Result :=
      [Point(l, t),
      Point(l+Floor(Width/2), t+Height-1),
      Point(l+Width, t)
      ];
    bpBottomLeft:
    Result :=
      [Point(l+Width-1, t+Floor(Height/2)),
       Point(l-1, t+Height),
       Point(l+Floor(Width/2),t)
      ];
    bpBottomRight:
    Result :=
      [Point(l, t+Floor(Height/2)),
       Point(l+Width, t+Height),
       Point(l+Floor(Width/2),t)
      ];
  end;
end;

//------------------------------------------------------------------------------

procedure TSizeBtn.DrawAt(l,t:integer{$IFDEF FPC};b:Graphics.TBitmap{$ENDIF});
begin
  {$IFDEF FPC}
  if Assigned(b) then
    case fTargetObj.fSizeCtrl.BtnShape of
     tszbSquare:
      b.Canvas.Rectangle(l,t, l+Width, t+Height);
     tszbTriangle:
      b.Canvas.Polygon(CalcTriangle(l,t));
     tszbCircle:
      b.Canvas.Ellipse(l,t, l+Width, t+Height);
     tszbMockTube:
     begin
      b.Canvas.Ellipse(l,t,l+Width,t+Height);
      b.Canvas.Polygon(CalcTriangle(l,t));
     end;
     tszbRoundRect:
      b.Canvas.RoundRect(l,t,l+Width,t+Height, (Width+Height) div 4,(Width+Height) div 4);
     tszbRhombus:
      b.Canvas.Polygon(
      [Point(l,t+Ceil(Height/2)-1),
      Point(l+Ceil(Width/2)-1, t),
      Point(l+Width-1,t+Ceil(Height/2)-1),
      Point(l+Ceil(Width/2)-1, t+Height-1)]);
  end
  else
  {$ENDIF}
  case fTargetObj.fSizeCtrl.BtnShape of
     tszbSquare:
      Canvas.Rectangle(l,t, l+Width, t+Height);
     tszbTriangle:
      Canvas.Polygon(CalcTriangle(l,t));
     tszbCircle:
      Canvas.Ellipse(l,t, l+Width, t+Height);
     tszbMockTube:
     begin
      Canvas.Ellipse(l,t,l+Width,t+Height);
      Canvas.Polygon(CalcTriangle(l,t));
     end;
     tszbRoundRect:
      Canvas.RoundRect(l,t,l+Width,t+Height, (Width+Height) div 4,(Width+Height) div 4);
     tszbRhombus:
      Canvas.Polygon(
      [Point(l,t+Ceil(Height/2)-1),
      Point(l+Ceil(Width/2)-1, t),
      Point(l+Width-1,t+Ceil(Height/2)-1),
      Point(l+Ceil(Width/2)-1, t+Height-1)]);
  end;
end;

//------------------------------------------------------------------------------

procedure TSizeBtn.PaintAs(l,t:integer);
{$IFDEF FPC}
var b:Graphics.TBitmap;
  {$ENDIF}
begin
  if Assigned(fImage.Graphic) and not(fImage.Graphic.Empty) then
  begin
    if fTargetObj.fSizeCtrl.StretchBtnImage then
      Canvas.StretchDraw(Rect(l,t, width, height), fImage.Graphic)
    else
    begin
      Canvas.Draw(l,t, fImage.Graphic);
     {$IFDEF FPC}
      Self.SetShape(fImage.Bitmap);
     {$ENDIF}
    end;
  end
  else
  {$IFDEF FPC}
  begin
   if
     (fPrevShape <> fTargetObj.fSizeCtrl.BtnShape)
     or
     (fPrevSize <> Self.Width) then
   begin
    fPrevShape := fTargetObj.fSizeCtrl.BtnShape;
    fPrevSize := Self.Width;
    b := Graphics.TBitmap.Create;
    b.Monochrome:=True;
    b.Width := Self.Width;
    b.Height:= Self.Height;
    b.Canvas.Brush.Color := clWhite;
    b.Canvas.Pen.Color := clWhite;
    Self.DrawAt(0,0,b);
    Self.SetShape(b);
    b.Free;
   end;
   Self.DrawAt(0,0,nil);
  end;
  {$ELSE}
   Self.DrawAt(0,0);
  {$ENDIF}
end;

//------------------------------------------------------------------------------

procedure TSizeBtn.Reset;
begin
  SetBounds(Left, Top, fTargetObj.fSizeCtrl.BtnSize, fTargetObj.fSizeCtrl.BtnSize);
  Refresh;
end;

//------------------------------------------------------------------------------

procedure TSizeBtn.DoPaint(Sender:TObject);
begin
  Canvas.Brush.Color := fColor;

  if fPen = clNone then
    Canvas.Pen.Color := fColor
  else
    Canvas.Pen.Color := fPen;

  {$IFDEF VER3UP}
  AlphaBlend := fTargetObj.fSizeCtrl.BtnAlphaBlend <> 255;
  AlphaBlendValue := fTargetObj.fSizeCtrl.BtnAlphaBlend;
  {$ELSE}
   if Canvas.Brush.Style = bsClear then
      Color := fSizeCtrl.fParentForm.Color;
  {$ENDIF}

  PaintAs(0,0);
end;

//------------------------------------------------------------------------------

procedure TSizeBtn.mEnter(Sender:TObject);
begin
  fHover := True;
  UpdateBtnCursorAndColor;
end;

//------------------------------------------------------------------------------

procedure TSizeBtn.mLeave(Sender:TObject);
begin
  fHover := False;
  if not fHoverDown then
    if Assigned( fTargetObj.fSizeCtrl.onButtonUnhover) then
      fTargetObj.fSizeCtrl.onButtonUnhover(Sender);

  UpdateBtnCursorAndColor;
end;

//------------------------------------------------------------------------------
//  TTargetObj methods
//------------------------------------------------------------------------------

constructor TTargetObj.Create(aSizeCtrl: TSizeCtrl; aTarget: TControl);
var
  i: TBtnPos;
begin
  inherited Create;
  fSizeCtrl := aSizeCtrl;
  fTarget := aTarget;
  {$IFDEF FPC}
  p := TPen.Create;
  br:= TBrush.Create;
  frw := TCustomControl(fsizeCtrl.fForm);
  {$ELSE}
  fPanels := TList.Create;
  fPanelsNames := TStringList.Create;
  {$ENDIF}
  for i := bpLeft to High(TBtnPos) do
    fBtns[i] := TSizeBtn.Create(self, i);
end;

//------------------------------------------------------------------------------

destructor TTargetObj.Destroy;
var
  i: TBtnPos;
  {$IFnDEF FPC}
  k: integer;
  {$ENDIF}
begin
  {$IFnDEF FPC}
  fPanelsNames.Clear;
  for k := 0 to fPanels.Count - 1 do
    TObject(fPanels[k]).Free();
  fPanels.Clear;

  fPanels.Free;
  fPanelsNames.Free;
  {$ELSE}
  p.Free;
  br.Free;
  {$ENDIF}
  for i := bpLeft to high(TBtnPos) do
  begin
    if fBtns[i] <> nil then
      fBtns[i].Free;
  end;
  inherited Destroy;
end;

//------------------------------------------------------------------------------

procedure TTargetObj.ReconfButtons;
var i: TBtnPos;
begin
  for i := bpLeft to high(TBtnPos) do
  begin
    if i > fSizeCtrl.LastBtn then
      fBtns[i].Hide
    else
      fBtns[i].Show;
  end;
end;

//------------------------------------------------------------------------------

procedure TTargetObj.Update;
var
  i: TBtnPos;
  {$IFNDEF FPC}
  st: boolean;
  {$ENDIF}
  parentForm: TCustomForm;
  tl: TPoint;
  bsDiv2: integer;
begin
    parentForm := fSizeCtrl.fParentForm;
  if not assigned(parentForm) then
    exit;
  //get topleft of Target relative to parentForm ...
  tl := GetBoundsAsScreenRect(fTarget).TopLeft;
  //topleft := fTarget.BoundsRect.TopLeft;
  tl := parentForm.ScreenToClient(tl);
  bsDiv2 := (fSizeCtrl.BtnSize div 2);

  for i := bpLeft to fSizeCtrl.LastBtn do
  begin
  if fBtns[i].ParentWindow <> parentForm.Handle then
  begin
    fBtns[i].ParentWindow := parentForm.Handle; //ie keep btns separate !!!
    {$IFNDEF FPC}
    fBtns[i].SetZOrder(true); //force btns to the top ...
    {$ENDIF}
    //just to be sure, that our button will be displayed correctly
    fBtns[i].Position := poDesigned;
    {$IFNDEF FPC}
    st := true;
    {$ENDIF}
  end;
    fBtns[i].Left := tl.X - bsDiv2;
    case i of
      bpTop, bpBottom:
        fBtns[i].Left := fBtns[i].Left + (fTarget.Width div 2);
      bpRight, bpTopRight, bpBottomRight:
        fBtns[i].Left := fBtns[i].Left + fTarget.Width - 1;
    end;
    fBtns[i].Top := tl.Y - bsDiv2;
    case i of
      bpLeft, bpRight:
        fBtns[i].Top := fBtns[i].Top + (fTarget.Height div 2);
      bpBottomLeft, bpBottom, bpBottomRight:
        fBtns[i].Top := fBtns[i].Top + fTarget.Height - 1;
    end;
    {$IFNDEF FPC}
    //force btns to the top ...
    if st then begin
      SetWindowPos(fBtns[i].Handle, HWND_TOP, fBtns[i].Left,
      fBtns[i].Top, fBtns[i].Left + fBtns[i].Width, fBtns[i].Left + fBtns[i].Top,
      SWP_NOACTIVATE or SWP_NOACTIVATE or SWP_NOMOVE or SWP_NOSIZE);
    end;
    {$ENDIF}
    if not fBtns[i].Visible then
    fBtns[i].Visible := True;
    {$IFDEF FPC}
    fBtns[i].Update;//reduce button flicker
    {$ENDIF}
  end;
end;

//------------------------------------------------------------------------------

procedure TTargetObj.DragMoveUpdate(const Rectange: TRect; const t: byte);
var
  i: TBtnPos;
  lLeft, lTop: integer;
  tl: TPoint;
  check: boolean;
  ilr, ilrd: TBtnPosSet;
  bsdiv2: integer;
begin
  //TL := TopLeft
  tl := Rectange.TopLeft;
  if  fSizeCtrl.BtnCount = szctrl4btns then
    ilr := [bpLeft, bpTop, bpRight, bpBottom]
  else
    ilr := [bpLeft, bpTop, bpRight, bpBottom, bpTopLeft, bpTopRight,
    bpBottomRight, bpBottomLeft];
  check := true;
  bsDiv2 := (fSizeCtrl.BtnSize div 2);
  case t of
    0: check := false;
    1:
    case fSizeCtrl.fCapturedBtnPos  of
    bpNone: check := false;
    bpLeft: ilrd := [bpTopLeft, bpLeft, bpBottomLeft];
    bpTop: ilrd := [bpTopLeft, bpTop, bpTopRight];
    bpRight: ilrd := [bpTopRight, bpRight, bpBottomRight];
    bpBottom: ilrd := [bpBottomLeft, bpBottom, bpBottomRight];
    bpTopLeft: ilrd := [bpTopRight, bpTop, bpTopLeft, bpLeft, bpBottomLeft];
    bpTopRight: ilrd := [bpTopLeft, bpTop, bpTopRight, bpRight, bpBottomRight];
    bpBottomRight: ilrd := [bpBottomRight, bpBottom, bpBottomLeft, bpRight, bpTopRight];
    bpBottomLeft: ilrd := [bpTopLeft, bpLeft, bpBottomLeft, bpBottom, bpBottomRight];
    end;
    2:
     if fSizeCtrl.fCapturedBtnPos <> bpNone
      then ilrd := [fSizeCtrl.fCapturedBtnPos]
     else if fSizeCtrl.ResizeIgnoreMethod = tszhMove then ilrd := ilr else exit;
  end;

  for i in ilr do
  begin
    if check and (not (i in ilrd)) then
    begin
      case fSizeCtrl.ResizeIgnoreMethod of
          tszhHide: fBtns[i].Hide
          else continue;
      end;
    end
    else if fBtns[i].Visible = False then
      fBtns[i].Show;
    lLeft := tl.X - bsDiv2;
    case i of
      bpTop, bpBottom:
        fBtns[i].Left := lLeft + (Rectange.Width div 2);
      bpRight, bpTopRight, bpBottomRight:
        fBtns[i].Left := lLeft + Rectange.Width - 1;
      else fBtns[i].Left := lLeft;
    end;
    lTop := tl.Y - bsDiv2;
    case i of
      bpLeft, bpRight:
        fBtns[i].Top := lTop + (Rectange.Height div 2);
      bpBottomLeft, bpBottom, bpBottomRight:
        fBtns[i].Top := lTop + Rectange.Height - 1;
      else fBtns[i].Top := lTop;
    end;
    {$IFDEF FPC}
    fBtns[i].Update;//reduce button flicker
    {$ENDIF}
  end;
  {$IFDEF FPC}
  fSizeCtrl.FForm.Update;//reduce panel flicker
  {$ENDIF}
end;

//------------------------------------------------------------------------------

procedure TTargetObj.StartFocus();
begin
  fFocusRect := fTarget.BoundsRect;
  {$IFDEF FPC};
  fDrawRect := True;
  fSizeCtrl.fForm.Update;
  {$ENDIF}
  fStartRec := fFocusRect;
end;

//------------------------------------------------------------------------------

function TTargetObj.MoveFocus(dx, dy: integer): boolean;
var
  L, T: integer;
begin
  if fTarget.Tag = fSizeCtrl.TagOptions.DenyChange then
    exit;

  L := fFocusRect.Left;
  T := fFocusRect.Top;
  fFocusRect := fStartRec;
  offsetRect(fFocusRect, dx, dy);
  Result := (L <> fFocusRect.Left) or (T <> fFocusRect.Top);
end;

//------------------------------------------------------------------------------

function TTargetObj.SizeFocus(dx, dy: integer; BtnPos: TBtnPos): boolean;
var
  L, T, R, B: integer;
begin
  if fTarget.Tag = fSizeCtrl.TagOptions.DenyChange then
    exit;

  L := fFocusRect.Left;
  T := fFocusRect.Top;
  R := fFocusRect.Right;
  B := fFocusRect.Bottom;

  fFocusRect := fStartRec;
  case BtnPos of
    bpLeft: Inc(fFocusRect.Left, dx);
    bpTopLeft:
    begin
      Inc(fFocusRect.Left, dx);
      Inc(fFocusRect.Top, dy);
    end;
    bpTop: Inc(fFocusRect.Top, dy);
    bpTopRight:
    begin
      Inc(fFocusRect.Right, dx);
      Inc(fFocusRect.Top, dy);
    end;
    bpRight: Inc(fFocusRect.Right, dx);
    bpBottomRight:
    begin
      Inc(fFocusRect.Right, dx);
      Inc(fFocusRect.Bottom, dy);
    end;
    bpBottom: Inc(fFocusRect.Bottom, dy);
    bpBottomLeft:
    begin
      Inc(fFocusRect.Left, dx);
      Inc(fFocusRect.Bottom, dy);
    end;
  end;
  Result := (L <> fFocusRect.Left) or (R <> fFocusRect.Right) or
    (T <> fFocusRect.Top) or (B <> fFocusRect.Bottom);
end;

//------------------------------------------------------------------------------

procedure TTargetObj.EndFocus;
var
  w, h: integer;
  i: TBtnPos;
begin
  for i := bpLeft to fSizeCtrl.fLastBtn do
    fBtns[i].dMouseUp;
  if not fSizeCtrl.ApplySizes then
  begin
    //update target position ...

    w := fFocusRect.Right - fFocusRect.Left;
    h := fFocusRect.Bottom - fFocusRect.Top;
    fFocusRect.Left := fTarget.Left - (fStartRec.Left - fFocusRect.Left);
    fFocusRect.Top := fTarget.Top - (fStartRec.Top - fFocusRect.Top);
    fFocusRect.Right := fFocusRect.Left + w;
    fFocusRect.Bottom := fFocusRect.Top + h;
  end;
    AlignCtrlToGrid(fTarget, Rect(fFocusRect.Left, fFocusRect.Top,
    fSizeCtrl.FixSize(fFocusRect.Right - fFocusRect.Left,0),
      fSizeCtrl.FixSize(fFocusRect.Bottom - fFocusRect.Top,1)));
  {$IFDEF FPC}
  fDrawRect := false;
  {$ENDIF}
  Update;
  fTarget.Invalidate;
end;

//------------------------------------------------------------------------------

procedure TTargetObj.DrawRect(obj: TControl);
var
  {$IFnDEF FPC}
  pr: TWinControl;
  panel: TMovePanel;
  s: string;
  k: integer;
  {$ENDIF}
  r: TRect;
begin
  if (fTarget.Tag = fSizeCtrl.TagOptions.DenyChange) {$IFDEF FPC} or not fDrawRect {$ENDIF}then
    exit;

  {$IFNDEF FPC}
  fLastRect := fFocusRect;
  pr := obj.Parent;
  s := IntToStr(integer(obj));

  k := Self.fPanelsNames.IndexOf(s);
  if k > -1 then
    panel := TMovePanel(fPanels[k])
  else
  begin
    panel := TMovePanel.Create(pr, fSizeCtrl);
    fPanelsNames.Add(IntToStr(integer(obj)));
    fPanels.Add(panel);
  end;

  panel.Width := fFocusRect.Right - fFocusRect.Left;
  panel.Height := fFocusRect.Bottom - fFocusRect.Top;

  panel.Left := fFocusRect.Left;
  panel.Top := fFocusRect.Top;
  {$ENDIF}
  r := TRect.Create( fFocusRect.TopLeft );
  r.BottomRight := fFocusRect.BottomRight;
  if {$IFDEF FPC}(fFocusRect <> fLastRect) and {$ENDIF} fSizeCtrl.ApplySizes then
  begin
    obj.Width := fSizeCtrl.FixSize(fFocusRect.Right - fFocusRect.Left,0);
    obj.Height := fSizeCtrl.FixSize(fFocusRect.Bottom - fFocusRect.Top,0);
    obj.Left := fFocusRect.Left;
    obj.Top := fFocusRect.Top;
    {$IFNDEF FPC}
     if fSizeCtrl.Parent.DoubleBuffered then
    begin
         fSizeCtrl.Parent.Update;
         obj.Repaint;
         panel.Repaint;
    end;
    {$ENDIF}
  end;
  //fix for poly-parenthed controls
  while obj.Parent <> fSizeCtrl.fParentForm  do
  begin
    obj := obj.Parent;
    r.TopLeft := obj.ClientToParent(r.TopLeft);
    r.BottomRight := obj.ClientToParent(r.BottomRight)
  end;
  {$IFDEF FPC}
  if fLastRect <> fFocusRect then
  {$ENDIF}
  case fSizeCtrl.ResizeFrameType of
    tszfButtons:
      self.DragMoveUpdate(r,0);
    tzfRButtons:
      self.DragMoveUpdate(r,1);
    tszfRButton:
      self.DragMoveUpdate(r,2);
  end;
  {$IFDEF FPC}
  if fSizeCtrl.ApplySizes then
  obj.Update;//reduce object flicker
  {$ELSE}
  if (panel.Visible = False) and (fSizeCtrl.ShowFrame) then
  begin
       panel.BringToFront;
       panel.Show;
  end;
  {$ENDIF}
  {$IFDEF FPC}
  if not fSizeCtrl.ShowFrame then
  Exit;
  if fLastRect <> fFocusRect then
  fSizeCtrl.FForm.Invalidate;//Finally, repaint the panel
  br.Assign(TCustomControl(fsizeCtrl.fForm).Canvas.Brush);
  p.Assign(TCustomControl(fsizeCtrl.fForm).Canvas.Pen);
  TCustomControl(fsizeCtrl.fForm).Canvas.Brush.Assign(fSizeCtrl.MovePanelCanvas.Brush);
  TCustomControl(fsizeCtrl.fForm).Canvas.Pen.Assign(fSizeCtrl.MovePanelCanvas.Pen);
  TCustomControl(fsizeCtrl.fForm).Canvas.Rectangle(fFocusRect);
  TCustomControl(fsizeCtrl.fForm).Canvas.Brush.Assign(br);
  TCustomControl(fsizeCtrl.fForm).Canvas.Pen.Assign(p);
  fLastRect := fFocusRect;
  {$ENDIF}
end;

//------------------------------------------------------------------------------
//  TSizeCtrl methods
//------------------------------------------------------------------------------

constructor TSizeCtrl.Create(TheOwner: TComponent);
begin
 if (TheOwner = nil) then
    raise Exception.Create('TSizeCtrl Owner is nil!!!');

 if not (TheOwner is TWinControl) then
    raise Exception.Create('TSizeCtrl.Create: Owner must be a TWinControl');
{$IFDEF FPC}
  inherited CreateNew(TheOwner);
 {$else}
  inherited Create(TheOwner);
{$ENDIF}
  fTargetList := TList.Create;
  fReIgnBtn := tszhNone;
  fRegList := TList.Create;
  fAlignToGrid := true;
  fCanv := TCanvas.Create;
  fCanv.Brush.Color := clBtnFace;
  fCanv.Pen.Color := clBlack;
  fBtnCount := TSizeCtrlBtnCount.szctrl8btns;
  fLastBtn := bpBottomLeft;
  fEditParent := False;

  fTags.DenySelect := 2012;
  fTags.AllowMove        := fTags.DenySelect + 1;
  fTags.AllowResize      := fTags.DenySelect + 2;
  fTags.AllowMultiResize := fTags.DenySelect + 3;
  fTags.AllowMultiMove   := fTags.DenySelect + 4;
  fTags.AllowMultiChange := fTags.DenySelect + 5;
  fTags.AllowMultiResize := fTags.DenySelect + 6;
  fTags.DenyMove         := fTags.DenySelect + 7;
  fTags.DenyResize       := fTags.DenySelect + 8;
  fTags.DenyChange       := fTags.DenySelect + 9;
  fTags.DenyMultiResize  := fTags.DenySelect + 10;
  fTags.DenyMultiMove    := fTags.DenySelect + 11;
  fTags.DenyMultiChange  := fTags.DenySelect + 12;
  fTags.DenyMultiResize  := fTags.DenySelect + 13;


  fTags.ChangeTop        := 1999;

  fTags.ChangeLeft       := fTags.ChangeTop + 1;
  fTags.ChangeWidth      := fTags.ChangeTop + 2;
  fTags.ChangeHeight     := fTags.ChangeTop + 3;
  fTags.ChangeTopHeight  := fTags.ChangeTop + 4;
  fTags.ChangeTopWidth   := fTags.ChangeTop + 5;
  fTags.ChangeLeftHeight := fTags.ChangeTop + 6;
  fTags.ChangeLeftWidth  := fTags.ChangeTop + 7;

  fTags.ChangeTopLeftWidth      := fTags.ChangeTop + 8;
  fTags.ChangeTopLeftHeight     := fTags.ChangeTop + 9;
  FTags.ChangeTopLeftWidthHeight:= fTags.ChangeTop + 10;

  fGridSize := 8;
  fBtnAlpha := 255;
  fMovePanelAlpha := 255;
  fBtnSize := 5;
  FSelKey := VK_SHIFT;
  //Multi-escape key
  fSelActionSPKey := VK_ESCAPE;

  //Tabulation key
  fSelToggleKey := VK_TAB;

  //Awwwrows keys
  fMoveLeftKey  := VK_LEFT;
  fMoveTopKey   := VK_LEFT + 1;
  fMoveRightKey := VK_LEFT + 2;
  fMoveBottomKey:= VK_LEFT + 3;

  FDGKey := VK_MENU;
  fCanv.Pen.Style := psDot;
  fCanv.Pen.Mode := pmCopy;
  fCanv.Pen.Width := 1;
  fCanv.Pen.Color := clBlack;
  fCanv.Brush.Style := bsSolid;
  fCanv.Brush.Color := clBtnFace;
  fGridWhite := clWhite;
  fGridBlack := clGray;
  fBtnShape := tszbCircle;
  fEnabledBtnColor := clAqua;
  fHoverBtnColor := clAqua;
  fDisabledBtnColor := clGray;
  fApplySizes:= False;
  fShowFrame := True;
  fReSizeType:= TReSizeFrameType.tszfNone;
  fBtnFrameColor := clBlue;
  fHoverBtnFrameColor := clBlue;
  fDisabledBtnFrameColor := clNone;
  fPanelImage := TPicture.Create;
  fBtnImage := TPicture.Create;
  fHoverBtnImage := TPicture.Create;
  fDisabledBtnImage := TPicture.Create;
  fStretchBtnImage := True;
  fStretchPanelImage := True;
  fMultiResize := True;

  FConstraints := TSizeConstraints.Create(TControl(Self));
  fValidBtns := [bpLeft, bpTopLeft, bpTop, bpTopRight, bpRight,
    bpBottomRight, bpBottom, bpBottomLeft];
  {$IFDEF FPC}
  fDefWindowProc := WindowProc;
  WindowProc := WinProc;
  {$ELSE}
  fHandle := AllocateHWnd(WinProc);
  {$ENDIF}
  fForm := TWinControl(TheOwner);
  {$IFDEF FPC}
  if fForm is TCustomControl then
     TCustomControl(fForm).OnPaint:=self.formPaint;
    Left := -1;
  Top := -1;
  Height := 0;
  Width := 0;
  Enabled := True;
  Visible := False;
  {$ELSE}
  if fForm is TForm then
    TForm(fForm).OnPaint := Self.formPaint
  {$ENDIF}
{$IFDEF VER3D}
  screen.Cursors[crSize] := loadcursor(hInstance, 'NSEW');
{$ENDIF}
end;

//------------------------------------------------------------------------------

destructor TSizeCtrl.Destroy;
begin
If Assigned(fOldWindowProc) then
//To prevent recursion with WindowProc'es
  fParentForm.WindowProc := fOldWindowProc;
  if assigned(fTargetList) then
  begin
    {$IFNDEF FPC}
    DeallocateHWnd(fHandle);
    {$ENDIF}
    UnRegisterAll;
    fTargetList.Free;
    fRegList.Free;
    FConstraints.Free;
    fCanv.Free;
    fPanelImage.Free;
    fBtnImage.Free;
    fHoverBtnImage.Free;
    fDisabledBtnImage.Free;
    {$IFNDEF FPC}
             fGrid.Free;
    {$ENDIF}
  end;

  if Assigned(fGridForm) then
    fGridForm := nil;

  inherited Destroy;
end;

//------------------------------------------------------------------------------

procedure TSizeCtrl.CreateGrid;
begin
      fGridForm := TForm.CreateNew(Self);
      fGridForm.Parent := fForm;
      fGridForm.Position := poDesigned;
      fGridForm.BorderStyle := bsNone;
      {$IFNDEF FPC}
      {$IFDEF VER3UP}
      fGridForm.Color := ((fGridWhite + fGridBlack) div 5) + 17 + 28;
      fGridForm.TransparentColorValue := fGridForm.Color;
      fGridForm.TransparentColor := True;
      {$ELSE}
      if fForm is TCustomForm then
        fGridForm.Color := TCustomForm(fForm).Color
      else
        fGridForm.Color := fParentForm.Color;
      {$ENDIF}
      {$ELSE}
      if fForm is TCustomControl then
        fGridForm.Color := TCustomControl(fForm).Color;
      {$ENDIF}
      fGridForm.Anchors := [akLeft, akTop, akRight, akBottom];
      fGridForm.FormStyle := fsNormal;
      fGridForm.OnPaint := Self.formPaint;
      fGridForm.SendToBack;
      fGridForm.Visible := False;
end;

//------------------------------------------------------------------------------

function TSizeCtrl.FixSize(input, wh: integer): integer;
begin
      if wh = 0 then
      begin
        Result := max(Constraints.MinWidth, input);
        if Constraints.MaxWidth > 0 then
          Result := min(Result, Constraints.MaxWidth);
      end
      else
      begin
        Result := max(Constraints.MinHeight, input);
        if Constraints.MaxHeight > 0 then
          Result := min(Result, Constraints.MaxHeight);
      end;
end;


//------------------------------------------------------------------------------

procedure TSizeCtrl.SetEnabled(Value: boolean);
var
  i: integer;
begin
  if Value = fEnabled then
    exit;

  fParentForm := GetParentForm(TWinControl(owner));
  if fParentForm = nil then
    exit;
  UpdateGrid;
  fEnabled := Value;
  ClearTargets;

  if fEnabled then
  begin
    //hook all registered controls and disable their OnClick events ...
    for i := 0 to fRegList.Count - 1 do
      TRegisteredObj(fRegList[i]).Hook;
    //hook the parent form too ...
    fOldWindowProc := fParentForm.WindowProc;//Backup actual WindProc
    fParentForm.WindowProc := Self.FormWindowProc;//Load modified WindProc
  end
  else
  begin
    //unhook all registered controls and reenable their OnClick events ...
    for i := 0 to fRegList.Count - 1 do
      TRegisteredObj(fRegList[i]).UnHook;
    //unhook the parent form too ...
    fParentForm.WindowProc := fOldWindowProc;
  end;
end;

procedure TSizeCtrl.FormWindowProc(var Msg: TMessage);
begin

  DoWindowProc(fOldWindowProc, Msg);
end;

//------------------------------------------------------------------------------

//TSizeCtrl's own message handler to process CM_CUSTOM_MSE_DOWN message
procedure TSizeCtrl.WinProc(var iMsg: TMessage);
var
  Button: TMouseButton;
  ShiftState: TShiftState;
begin
  with iMsg do
    if Msg = CM_LMOUSEDOWN then
      try
        fLMouseDownPending := False;
        if bool(WParam) then
          Button := mbLeft
        else
          Button := mbRight;
        if bool(LParam) then
          ShiftState := [ssShift]
        else
          ShiftState := [];
        DoMouseDown(nil, Button, ShiftState);
      except
        Application.HandleException(Self);
      end
    else
     {$IFDEF FPC}
      fDefWindowProc(iMsg);
     {$ELSE}
      Result := DefWindowProc(fHandle, Msg, wParam, lParam);
     {$ENDIF}
end;

//------------------------------------------------------------------------------

function TSizeCtrl.KeysToShiftState(Keys: Word): TShiftState;
begin
  Result := [];
  if KeyIsPressed( Self.FSelKey ) then Include(Result, ssShift);
  if Keys and MK_CONTROL <> 0 then Include(Result, ssCtrl);
  if Keys and MK_LBUTTON <> 0 then Include(Result, ssLeft);
  if Keys and MK_RBUTTON <> 0 then Include(Result, ssRight);
  if Keys and MK_MBUTTON <> 0 then Include(Result, ssMiddle);
  if KeyIsPressed( Self.FDGKey ) then Include(Result, ssAlt);
end;

//------------------------------------------------------------------------------

function TSizeCtrl.KeyDataToShiftState(): TShiftState;
begin
  Result := [];
  if KeyIsPressed( Self.FSelKey ) then Include(Result, ssShift);
  if GetKeyState(VK_CONTROL) < 0 then Include(Result, ssCtrl);
  if KeyIsPressed( Self.FDGKey ) then Include(Result, ssAlt);
end;

//------------------------------------------------------------------------------

//WindowProc for the 'hooked' form and all 'hooked' controls
procedure TSizeCtrl.DoWindowProc(DefaultProc: TWndMethod; var Msg: TMessage);
var
  i
  {$IFDEF FPC}
  :integer;
  {$ELSE}
  ,k: integer;
  list: TList;
  {$ENDIF}
  ShiftState: TShiftState;
  controlPt, screenPt: TPoint;
  regCtrl: TControl;
  handled: boolean;

  //this seems the only reasonably simple way of managing both 'owned' and
  //'notified' WM_LBUTTONDOWN messages ...
  procedure PostMouseDownMessage(isLeftBtn, shiftKeyPressed: boolean;
  isVirtual:boolean=false);
  begin
    if fLMouseDownPending then
      exit;

    if assigned(fOnMouseDown) then
    begin
      getCursorPos(screenPt);
      regCtrl := RegisteredCtrlFromPt(screenPt, nil);
      if assigned(regCtrl) then
      begin
        {$IFDEF FPC}
        if isVirtual and self.isTarget(regCtrl) then Exit;
        {$ENDIF}
        handled := False;
        controlPt := regCtrl.ScreenToClient(screenPt);
        fOnMouseDown(self, regCtrl, controlPt, handled);

        if handled then
          exit;
      end else if isVirtual then exit;

    end;

    fLMouseDownPending := True;
    PostMessage(
    {$IFDEF FPC}
    Handle
    {$ELSE}
    fHandle
    {$ENDIF}, CM_LMOUSEDOWN, Ord(isLeftBtn), Ord(shiftKeyPressed));
  end;

begin
  case Msg.Msg of

    WM_MOUSEFIRST .. WM_MOUSELAST:
    begin
      ShiftState := Self.KeysToShiftState(word(TWMMouse(Msg).Keys));
      case Msg.Msg of
        WM_LBUTTONDOWN: PostMouseDownMessage(True, ssShift in ShiftState);
        WM_RBUTTONDOWN: DoPopupMenuStuff;
        WM_MOUSEMOVE: DoMouseMove(nil, ShiftState);
        WM_LBUTTONUP: DoMouseUp(nil, mbLeft, ShiftState);
        //Could also add event handlers for right click events here.
      end;
      Msg.Result := 0;
    end;

    WM_PARENTNOTIFY:
    {$IFDEF FPC}
    if not (TWMParentNotify(Msg).NotifyCode in [WM_CREATE, WM_DESTROY]) then
    {$ELSE}
      if not (TWMParentNotify(Msg).Event in [WM_CREATE, WM_DESTROY]) then
    {$ENDIF}
      begin
        if KeyIsPressed(Self.FSelKey) then
          ShiftState := [ssShift]
        else
          ShiftState := [];
        {$IFDEF FPC}
        if((TWMParentNotify(Msg).NotifyCode = WM_LBUTTONDOWN)) then
        {$ELSE}
        if((TWMParentNotify(Msg).Event = WM_LBUTTONDOWN)) then
        {$ENDIF}
           PostMouseDownMessage(True, ssShift in ShiftState, true);
        Msg.Result := 0;
      end;

    WM_SETCURSOR:
    Begin
      if (HIWORD(Msg.lParam) <> 0) then
      begin
        {$IFNDEF FPC} //Because of different cursor changing realizations
                      //We need to change messages from different places
        Msg.Result := 1;
        {$ENDIF}
        getCursorPos(screenPt);
        regCtrl := RegisteredCtrlFromPt(screenPt);

        handled := False;
        if assigned(fOnSetCursor) and assigned(regCtrl) then
        begin
          controlPt := regCtrl.ScreenToClient(screenPt);
          fOnSetCursor(self, RegisteredCtrlFromPt(screenPt), controlPt, handled);
        end;

        if not IsValidMove and handled and (TargetIndex(regCtrl) >= 0) then
        begin
            if Assigned(DefaultProc) then
            DefaultProc(Msg);
        end
        else if not handled and assigned(regCtrl) then
        begin
          {$IFDEF FPC}
            Msg.Result := 1;
            THackedControl(regCtrl).SetCursor(crHandPoint);
            Self.SetCursor(crHandPoint);
          {$ELSE}
            Windows.SetCursor(screen.Cursors[crHandPoint]);
          {$ENDIF}
        end
        else
          DefaultProc(Msg);
      end
      else
        DefaultProc(Msg);
    End;

    WM_GETDLGCODE: Msg.Result :=  DLGC_WANTTAB or DLGC_WANTARROWS;
    WM_KEYDOWN:
    begin
      Msg.Result := 0;
      {$IFDEF FPC}
      if DoKeyDown(TLMKey(Msg)) then
        exit;
      {$ELSE}
      if DoKeyDown(TWMKey(Msg)) then
        exit;
      {$ENDIF}
      if Self.fCapturedBtnPos = bpNone then
      begin
      if Msg.WParam = fMoveTopKey then
        begin
          {$IFDEF FPC}
          Msg.Result := 1;
          {$ENDIF}
          if KeyIsPressed(Self.FSelKey) then
          begin
            SizeTargets(0, -GridSize);
            if assigned(fEndEvent) then
              fEndEvent(self, scsSizing);
          end
          else
          begin
            MoveTargets(0, -GridSize);
            if assigned(fEndEvent) then
              fEndEvent(self, scsMoving);
          end;
        end;
        if Msg.WParam = fMoveBottomKey then
        begin
          {$IFDEF FPC}
          Msg.Result := 1;
          {$ENDIF}
          if KeyIsPressed(Self.FSelKey) then
          begin
            SizeTargets(0, +GridSize);
            if assigned(fEndEvent) then
              fEndEvent(self, scsSizing);
          end
          else
          begin
            MoveTargets(0, +Gridsize);
            if assigned(fEndEvent) then
              fEndEvent(self, scsMoving);
          end;
        end;
        if Msg.WParam = fMoveLeftKey then
        begin
          {$IFDEF FPC}
          Msg.Result := 1;
          {$ENDIF}
          if KeyIsPressed(Self.FSelKey) then
          begin
            SizeTargets(-GridSize, 0);
            if assigned(fEndEvent) then
              fEndEvent(self, scsSizing);
          end
          else
          begin
            MoveTargets(-GridSize, 0);
            if assigned(fEndEvent) then
              fEndEvent(self, scsMoving);
          end;
        end;
        if Msg.WParam = fMoveRightKey then
        begin
          {$IFDEF FPC}
          Msg.Result := 1;
          {$ENDIF}
          if KeyIsPressed(Self.FSelKey) then
          begin
            SizeTargets(+GridSize, 0);
            if assigned(fEndEvent) then
              fEndEvent(self, scsSizing);
          end
          else
          begin
            MoveTargets(+GridSize, 0);
            if assigned(fEndEvent) then
              fEndEvent(self, scsMoving);
          end;
        end;
        if Msg.WParam = fSelToggleKey then
        begin
          {$IFDEF FPC}
          Msg.Result := 1;
          {$ENDIF}
          if fRegList.Count = 0 then
            exit
          else if targetCount = 0 then
            AddTarget(TRegisteredObj(fRegList[0]).fControl)
          else
          begin
            i := RegisteredIndex(Targets[0]);
            if KeyIsPressed(Self.FSelKey) then
              Dec(i)
            else
              Inc(i);
            if i < 0 then
              i := fRegList.Count - 1
            else if i = fRegList.Count then
              i := 0;
            ClearTargets;
            AddTarget(TRegisteredObj(fRegList[i]).fControl);
          end;
        end;
        if Msg.WParam = fSelActionSPkey then
        begin
          {$IFDEF FPC}
          Msg.Result := 1;
          {$ENDIF}
          //ESCAPE is used for  -
          //  1. selecting the parent of the currenctly selected target
          if fState = scsReady then
          begin
            if (targetCount = 0) then
              exit;
            i := RegisteredIndex(Targets[0].Parent);
            ClearTargets;
            if i >= 0 then
              AddTarget(TRegisteredObj(fRegList[i]).fControl);
          end;
        end;
      end;
    end;

    WM_KEYUP: Msg.Result := 0;
    WM_CHAR: Msg.Result := 0;
    WM_CANCELMODE: //Cancel all modal operations
    Begin
    //What is that? Do you ask me? You'd better ask Embarcadero,
      //Because they do not supplied truely focus-lose event
      //P.s thanks to "furious programming" from Lazarus forum,
      //Because now i'm know, that in FP i do not have to
      //do this...

      if ( GetForegroundWindow <> fParentForm.Handle)
          and (fState <> scsReady) then
      begin
        ShowMessage('s');
        fState := scsReady;
        if TargetCount > 0 then
        for i := 0 to TargetCount - 1 do
          begin
            TTargetObj(fTargetList[i]).EndFocus;
            {$IFnDEF FPC}
            TTargetObj(fTargetList[i]).fPanelsNames.Clear;
            list := TTargetObj(fTargetList[i]).fPanels;
            for k := 0 to list.Count - 1 do
              TObject(list[k]).Free();
            list.Clear;
            {$ENDIF}
          end;
          Msg.Result := 0;
        end;
    end;
    else if Assigned(DefaultProc) then
      DefaultProc(Msg);
  end;
end;

//------------------------------------------------------------------------------
{$IFDEF FPC}
function TSizeCtrl.DoKeyDown(var Message: TLMKey): boolean;
{$ELSE}
function TSizeCtrl.DoKeyDown(var Message: TWMKey): boolean;
{$ENDIF}
var
  ShiftState: TShiftState;
begin
  Result := True;
  {$IFDEF FPC}
  if fParentForm.KeyPreview and THackedWinControl(fParentForm).DoRemainingKeyDown(Message) then
    Exit;
  {$ELSE}
  if fParentForm.KeyPreview and THackedWinControl(fParentForm).DoKeyDown(Message) then
    Exit;
  {$ENDIF}
  if Assigned(fOnKeyDown) then
    with Message do
    begin
      ShiftState := Self.KeyDataToShiftState();
      fOnKeyDown(Self, CharCode, ShiftState);
      if CharCode = 0 then
        Exit;
    end;
  Result := False;
end;

//------------------------------------------------------------------------------
function tSizeCtrl.isTarget(tcrl: TControl): boolean;
var t,id: integer;
begin
  id := integer(tcrl);
  Result := False;
  if not fTargetList.Count = 0 then
  for t := 0 to fTargetList.Count -1  do
    if integer(TTargetObj(fTargetList[t]).fTarget) = id then
      begin
        Result := True;
        Exit;
      end;
end;
function TSizeCtrl.GetTargets(index: integer): TControl;
begin
  if (index < 0) or (index >= TargetCount) then
    Result := nil
  else
    Result := TTargetObj(fTargetList[index]).fTarget;
end;

//------------------------------------------------------------------------------

function TSizeCtrl.TargetIndex(Control: TControl): integer;
var
  i: integer;
begin

  Result := -1;
  if assigned(Control) then
    for i := 0 to fTargetList.Count - 1 do
      if TTargetObj(fTargetList[i]).fTarget = Control then
      begin
        Result := i;
        break;
      end;
end;

//------------------------------------------------------------------------------

function TSizeCtrl.AddTarget(Control: TControl): integer;
var
  TargetObj: TTargetObj;
begin

  Result := -1;
  if (csDestroying in ComponentState) or (fState <> scsReady) then
    exit;

  if ((integer(Control) = integer(FForm)) and not fEditParent) or (fTargetList.Count > 0) and (Control.Tag = fTags.DenyMultiChange) then
    exit;
  Result := TargetIndex(Control);
  if not assigned(Control) or not Control.Visible or (integer(Control) = integer(fParentForm))
   or
    (Result >= 0) then
    exit;
  Result := fTargetList.Count;
  TargetObj := TTargetObj.Create(self, Control);
  fTargetList.Add(TargetObj);
  RegisterControl(Control);

  fParentForm.ActiveControl := nil;

  UpdateBtnCursors;
  TargetObj.Update;
  {$IFNDEF FPC}
  TargetObj.Update; //b.f: TCustomForm cannot be placed right after showing
                    //Delphi VCL bug?
                    //YES<
  {$ENDIF}
  if assigned(fTargetChangeEvent) then
    fTargetChangeEvent(self);
end;

//------------------------------------------------------------------------------

function TSizeCtrl.PerformRecursively
(
  Control: TControl;
  Func: byte;
  FullRecursion: boolean;
  Include:boolean;
  AddType:TRecursionVector
): boolean;
var obj: TWinControl;
    i: integer;
    procedure fff(ac: TControl; t: byte; var r: boolean);
    begin
      case t of
          0:   If(AddTarget(ac)=-1)        then r:=false;
          1:   If(DeleteTarget(ac)=-1)     then r:=false;
          2:   If(RegisterControl(ac)=-1)  then r:=false;
          3: If(UnRegisterControl(ac)=-1)then r:=false;
      end;
    end;
begin

  if (csDestroying in ComponentState) or (fState <> scsReady) then
  begin
    Result := false;
    exit;
  end;
  Result := true;
  obj := TWinControl(Control);
  case AddType of
    trecChild:
    begin
      if Include then
        fff(obj,func,Result);
      if obj.ControlCount > 0 then
      begin
        for i := 0 to obj.ControlCount-1 do
        begin
           fff(obj.Controls[i],func,Result);
           if FullRecursion then
              if not Self.PerformRecursively(obj.Controls[i],Func,True,False,AddType)
              then Result := False;
        end;
      end;
    end;
    trecParent:
    begin
      if Include then
         fff(obj,func,Result);
      while (obj <> FForm) and Assigned(obj)  do
      begin
        obj := obj.Parent;
        if obj <> FForm then
        begin
          fff(obj,func,Result);
           if FullRecursion then
              if not Self.PerformRecursively(obj,Func,True,False,AddType)
              then Result := false;
        end;
      end;
    end;
    trecBoth:
    Begin
      if Include then
        fff(obj,func,Result);
      if obj.ControlCount > 0 then
      begin
        for i := 0 to obj.ControlCount-1 do
        begin
           fff(obj.Controls[i],func,Result);
            if FullRecursion then
              if not Self.PerformRecursively(obj.Controls[i],Func,True,False,TRecursionVector.trecChild)
              then Result := False;
        end;
      end;
      obj := obj.Parent;
      while (obj <> FForm) and Assigned(obj)  do
      begin
        if obj <> FForm then
        begin
          fff(obj,func,Result);
           if FullRecursion then
              if not Self.PerformRecursively(obj,Func,True,False,TRecursionVector.trecParent)
              then Result := false;
        end;
        obj := obj.Parent;
      end;
    End;

  end;

end;

//------------------------------------------------------------------------------

function TSizeCtrl.AddTargetR
(
  Control: TControl;
  AddType: TRecursionVector = trecChild;
  FullRecursion: Boolean = True
): boolean;
begin
    Result := Self.PerformRecursively
    (
      Control,
      0,
      FullRecursion,
      True,
      AddType
    );
end;

//------------------------------------------------------------------------------

function TSizeCtrl.DeleteTarget(Control: TControl): integer;
var
  i: integer;
begin
  Result := -1;
  i := TargetIndex(Control);
  if i < 0 then
    exit;
  Result := i;
  TTargetObj(fTargetList[i]).Free;
  fTargetList.Delete(i);
  UpdateBtnCursors;
  if assigned(fTargetChangeEvent) then
    fTargetChangeEvent(self);
end;

//------------------------------------------------------------------------------

function TSizeCtrl.DeleteTargetR
(
  Control: TControl;
  AddType: TRecursionVector = trecChild;
  FullRecursion: Boolean = True
): boolean;
begin
    Result := Self.PerformRecursively
    (
      Control,
      1,
      FullRecursion,
      True,
      AddType
    );
end;

//------------------------------------------------------------------------------

procedure TSizeCtrl.ClearTargets;
var
  i: integer;
begin
  if fTargetList.Count = 0 then
    exit;
  for i := 0 to fTargetList.Count - 1 do
  begin
    if fTargetList[i] <> nil then
      TTargetObj(fTargetList[i]).Free;
  end;

  fTargetList.Clear;
  if (csDestroying in ComponentState) then
    exit;
  UpdateBtnCursors;
  if assigned(fTargetChangeEvent) then
    fTargetChangeEvent(self);
end;

//------------------------------------------------------------------------------

function TSizeCtrl.RegisterControl(Control: TControl): integer;
var
  RegisteredObj: TRegisteredObj;
begin
  if
  {$IFNDEF FPC}Control is TMovePanel or{$ENDIF}
  (integer(Control) = integer(Self))
  or ((integer(Control) = integer(FForm)) and (not fEditParent)) or
  (integer(Control) = integer(fParentForm)) or
  (Assigned(fGridForm) and (integer(Control) = integer(fGridForm))) then //b.f with 1000 objects selected
    Exit;
  if Control.Tag = fTags.DenySelect then Exit;
  if RegisteredIndex(Control) >= 0 then
    exit;

  Result := fRegList.Count;
  RegisteredObj := TRegisteredObj.Create(self, Control);
  fRegList.Add(RegisteredObj);
end;

//------------------------------------------------------------------------------

function TSizeCtrl.RegisterControlR
(
  Control: TControl;
  AddType: TRecursionVector = trecChild;
  FullRecursion: Boolean = True
): boolean;
begin
    Result := Self.PerformRecursively
    (
      Control,
      2,
      FullRecursion,
      True,
      AddType
    );
end;

//------------------------------------------------------------------------------

function TSizeCtrl.UnRegisterControl(Control: TControl): integer;
var
  i: integer;
begin
  Result := -1;
  //first, make sure it's not a current target ...
  DeleteTarget(Control);
  //now unregister it ...
  i := RegisteredIndex(Control);
  if i < 0 then
    exit;
  Result := i;
  TRegisteredObj(fRegList[i]).Free;
  fRegList.Delete(i);
end;

//------------------------------------------------------------------------------

function TSizeCtrl.UnRegisterControlR
(
  Control: TControl;
  AddType: TRecursionVector = trecChild;
  FullRecursion: Boolean = True
): boolean;
begin
    Result := Self.PerformRecursively
    (
      Control,
      3,
      FullRecursion,
      True,
      AddType
    );
end;

//------------------------------------------------------------------------------

procedure TSizeCtrl.UnRegisterAll;
var
  i: integer;
begin
  //first, clear any targets
  ClearTargets;
  if fRegList.Count = 0 then
                            Exit;
  //now, clear all registered controls ...
  for i := 0 to fRegList.Count - 1 do
  begin
    if Assigned(fRegList[i]) then
      TRegisteredObj(fRegList[i]).Free;
  end;
  fRegList.Clear;
end;

//------------------------------------------------------------------------------

function TSizeCtrl.RegisteredIndex(Control: TControl): integer;
var
  i: integer;
begin
  Result := -1;
  if fRegList.Count = 0 then
                            Exit;
  for i := 0 to fRegList.Count - 1 do
    if TRegisteredObj(fRegList[i]).fControl = Control then
    begin
      Result := i;
      break;
    end;
end;

//------------------------------------------------------------------------------

function TSizeCtrl.TargetCtrlFromPt(screenPt: TPoint): TControl;
var
  i: integer;
  tmpCtrl: TWinControl;
begin
  //nb: If controls overlap at screenPt, then the (top-most) child control
  //is selected if there is a parent-child relationship. Otherwise, simply
  //the first control under screenPt is returned.
  Result := nil;
  for i := fTargetList.Count - 1 downto 0 do
    with TTargetObj(fTargetList[i]) do
    begin
      if not PointIsInControl(screenPt, fTarget) then
        continue;
      if not (fTarget is TWinControl) then
      begin
        Result := fTarget;
        exit; //ie assume this is top-most since it can't be a parent.
      end
      else if not assigned(Result) then
        Result := fTarget
      else
      begin
        tmpCtrl := TWinControl(fTarget).Parent;
        while assigned(tmpCtrl) and (tmpCtrl <> Result) do
          tmpCtrl := tmpCtrl.Parent;
        if assigned(tmpCtrl) then
          Result := fTarget;
      end;
    end;
end;

//------------------------------------------------------------------------------

function TSizeCtrl.getRegObj(C: TComponent): TRegisteredObj;
var
  i: integer;
begin
  for i := fRegList.Count - 1 downto 0 do
  begin
    Result := TRegisteredObj(fRegList[i]);
    if (integer(Result.fControl) = integer(C)) then
      exit;
  end;
  Result := nil;
end;

//------------------------------------------------------------------------------

function TSizeCtrl.RegisteredCtrlFromPt(screenPt: TPoint;
  ParentX: TWinControl = nil): TControl;
var
  i: integer;
  rO: TRegisteredObj;
  tmp: TControl;
begin
  //nb: If controls overlap at screenPt, then the (top-most) child control
  //is selected if there is a parent-child relationship. Otherwise, simply
  //the first control under screenPt is returned.

  if (ParentX = nil) then
    ParentX := fForm;

  // for i := fRegList.Count -1 downto 0 do
  for i := ParentX.ControlCount - 1 downto 0 do
  begin

    rO := self.getRegObj(ParentX.Controls[i]);

    if rO = nil then
      continue;

    if rO.fControl.Parent is TTabSheet then
    begin
      if not rO.fControl.Parent.Visible then
        continue;
    end;

    with rO do
    begin

      if not PointIsInControl(screenPt, fControl) or not fControl.Visible then //b.f
        continue;

      Result := fControl;

      if (Result is TPageControl) then
      begin

        tmp := RegisteredCtrlFromPt(screenPt, TPageControl(Result).ActivePage);

        if (tmp <> nil) then
        begin

          Result := tmp;
        end;
      end;

      if (Result is TWinControl) then
      begin
        tmp := RegisteredCtrlFromPt(screenPt, (Result as TWinControl));
        if (tmp <> nil) then
          Result := tmp;
      end;

      exit;
    end;
  end;

  Result := nil;
end;

//------------------------------------------------------------------------------

function TSizeCtrl.GetTargetCount: integer;
begin
  Result := fTargetList.Count;
end;

//------------------------------------------------------------------------------

procedure TSizeCtrl.MoveTargets(dx, dy: integer);
var
  i, RDx, RDy: integer;
begin
  if not IsValidMove then exit;
  if (dx = 0) and (dy = 0) then
    exit;
  if ((dx = 1) or (dy = 1)) and (GridSize <> 1) then
     begin
       Inc(dx,1);
       Inc(dy,1);
     end;
  if ((dx = -1) or (dy = -1)) and (GridSize <> 1) then
     begin
       Dec(dx,1);
       Dec(dy,1);
     end;
  for i := 0 to fTargetList.Count - 1 do
    with TTargetObj(fTargetList[i]) do
    begin
    RDx :=  fTarget.Left + dx;
    RDy :=  fTarget.Top  + dy;
    if self.fAlignToGrid then
     begin
       if dx <> 0 then
       RDx := RDx - (RDx mod GridSize);
       if dy <> 0 then
       RDy := RDy - (RDy mod GridSize);
     end;
      if Xyweck(RDx,RDy,0,fTarget.Tag) then continue;
        fTarget.SetBounds(RDx,RDy, fTarget.Width, fTarget.Height);

       {$IFDEF FPC}
       fForm.Update;
       fTarget.Update;
       fTarget.Invalidate;
       {$ENDIF}
       Update;
    end;
end;

//------------------------------------------------------------------------------

procedure TSizeCtrl.SizeTargets(dx, dy: integer);
var
  i, RDx, RDy: integer;
begin
  if (dx <> 0) and not (IsValidSizeBtn(bpLeft) or IsValidSizeBtn(bpRight)) then
    exit;
  if (dy <> 0) and not (IsValidSizeBtn(bpBottom) or IsValidSizeBtn(bpTop)) then
    exit;
  if (dx = 0) and (dy = 0) then
    exit;
  if ((dx = 1) or (dy = 1)) and (GridSize <> 1) then
     begin
       Inc(dx,1);
       Inc(dy,1);
     end;
  if ((dx = -1) or (dy = -1)) and (GridSize <> 1) then
     begin
       Dec(dx,1);
       Dec(dy,1);
     end;
  for i := 0 to fTargetList.Count - 1 do
    with TTargetObj(fTargetList[i]) do
    begin

    RDx :=  fTarget.Width + dx;
    RDy :=  fTarget.Height + dy;
    if self.fAlignToGrid then
     begin
       if dx <> 0 then
       RDx := RDx - (RDx mod GridSize);
       if dy <> 0 then
       RDy := RDy - (RDy mod GridSize);
     end;
    RDx := FixSize(RDx,0);
    RDy := FixSize(RDy,1);
    if MoveOnly then
    begin
       if not XyweckPositive(RDx,RDy,1,fTarget.Tag) then continue;
    end
    else
    begin
      if Xyweck(RDx,RDy,1,fTarget.Tag) then continue;
    end;
        fTarget.SetBounds(fTarget.Left, fTarget.Top, RDx,RDy);
      Update;
      {$IFDEF FPC}
        fTarget.Invalidate;
      {$ENDIF}
    end;
end;

//------------------------------------------------------------------------------

procedure TSizeCtrl.Update;
var
  i: integer;
begin
  for i := 0 to fTargetList.Count - 1 do
    TTargetObj(fTargetList[i]).Update;
end;

procedure TSizeCtrl.UpdateBtns;
var
  i: integer;
  x: TBtnPos;
begin
  for i := 0 to fTargetList.Count - 1 do
    for x := bpLeft to LastBtn do
      TTargetObj(fTargetList[i]).fBtns[x].UpdateBtnCursorAndColor;
end;

//------------------------------------------------------------------------------

procedure TSizeCtrl.DrawRect;
var
  i: integer;
begin
  if TargetCount = 0 then
    exit;
  for i := 0 to TargetCount - 1 do
      TTargetObj(fTargetList[i]).DrawRect(TTargetObj(fTargetList[i]).fTarget);
end;

//------------------------------------------------------------------------------

procedure TSizeCtrl.DoMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState);
var
  i, targetIdx: integer;
  p: TWinControl;
  parentClientRec: TRect;
  targetObj: TTargetObj;
begin
  fEscCancelled := False;
  GetCursorPos(fStartPt);

  fParentForm.ActiveControl := nil;

  if (Sender is TSizeBtn) then
  begin
    if fMoveOnly then
      exit; //should never happen
    targetObj := TSizeBtn(Sender).fTargetObj;
    fCapturedCtrl := targetObj.fTarget;
    fCapturedBtnPos := TSizeBtn(Sender).fPos;
    //make sure we're allowed to size these targets with this button ...
    if not IsValidSizeBtn(fCapturedBtnPos) then
      exit;
    fState := scsSizing;
  end
  else
  begin
    if (fState = TSCState.scsSizing) and (fCapturedBtnPos <> bpNone) then //b.f
    Exit;

    fCapturedBtnPos := bpNone;
    //First find the top-most control that's clicked ...
    //nb: It's so much simpler to do this here than try and work it out from
    //the WindowProc owner (because of disabled controls & non-TWinControls.)
    fCapturedCtrl := RegisteredCtrlFromPt(fStartPt);

    targetIdx := TargetIndex(fCapturedCtrl);
    if not (ssShift in Shift) and (targetIdx < 0) then
      ClearTargets;
    if not assigned(fCapturedCtrl) then
      exit;

    //if the control isn't a target then add it ...
    if targetIdx < 0 then
    begin

      AddTarget(fCapturedCtrl);
      exit;
      //if the control's already a target but the Shift key's pressed then delete it ...
    end
    else if (ssShift in Shift) then
    begin
      DeleteTarget(fCapturedCtrl);
      fCapturedCtrl := nil;
      exit;
    end;
    fParentForm.ActiveControl := nil;
    if
      (fCapturedCtrl.Tag = fTags.DenyMove)
      or
      (fCapturedCtrl.Tag = fTags.DenyChange)
      then
      Exit;
    if not IsValidMove then
      exit;
    targetObj := TTargetObj(fTargetList[targetIdx]);

    fState := scsMoving;
    {$IFDEF FPC}
    THackedControl(fCapturedCtrl).SetCursor(crSize);
    {$ELSE}
     Windows.SetCursor(Screen.Cursors[crSizeAll]);
    {$ENDIF}
  end;


  for i := 0 to TargetCount - 1 do
  begin
    TTargetObj(fTargetList[i]).StartFocus();
  end;

  if assigned(fStartEvent) then
    fStartEvent(self, fState);

  //now calculate and set the clipping region in screen coords ...
  p := targetObj.fTarget.Parent;
  parentClientRec := p.ClientRect;
  parentClientRec.TopLeft := p.ClientToScreen(parentClientRec.TopLeft);
  parentClientRec.BottomRight := p.ClientToScreen(parentClientRec.BottomRight);
  if fState = scsMoving then
  begin
    fClipRec := parentClientRec;
  end
  else
    with targetObj do //ie sizing
    begin
      fClipRec := fFocusRect;
      case TSizeBtn(Sender).fPos of
        bpLeft: fClipRec.Left := parentClientRec.Left;
        bpTopLeft:
        begin
          fClipRec.Left := parentClientRec.Left;
          fClipRec.Top := parentClientRec.Top;
        end;
        bpTop: fClipRec.Top := parentClientRec.Top;
        bpTopRight:
        begin
          fClipRec.Right := parentClientRec.Right;
          fClipRec.Top := parentClientRec.Top;
        end;
        bpRight: fClipRec.Right := parentClientRec.Right;
        bpBottomRight:
        begin
          fClipRec.Right := parentClientRec.Right;
          fClipRec.Bottom := parentClientRec.Bottom;
        end;
        bpBottom: fClipRec.Bottom := parentClientRec.Bottom;
        bpBottomLeft:
        begin
          fClipRec.Left := parentClientRec.Left;
          fClipRec.Bottom := parentClientRec.Bottom;
        end;
      end;
    end;

  DrawRect;
  THackedControl(fCapturedCtrl).MouseCapture := True;
end;

//------------------------------------------------------------------------------

procedure TSizeCtrl.DoMouseMove(Sender: TObject; Shift: TShiftState);
var
  i, dx, dy: integer;
  {$IFDEF FPC}
  {%H-}newPt: TPoint;
  {$ELSE}
  newPt:    TPoint;
  {$ENDIF}
  Q, R: integer;
begin

  if (fState = scsReady) or not assigned(fCapturedCtrl) then
    exit;
 // DrawRect;
  {$IFDEF FPC}
  GetCursorPos({%H-}newPt);
  {$ELSE}
  GetCursorPos(newPt);
  {$ENDIF}
  dx := newPt.X - fStartPt.X;
  dy := newPt.Y - fStartPt.Y;
  Q := 0;
  R := 0;

  if (fState = scsSizing) then
  begin
    case fCapturedBtnPos of
      bpLeft, bpRight: dy := 0;
      bpTop, bpBottom: dx := 0;
    end;

    if (not KeyIsPressed(Self.FDGKey)) and self.fAlignToGrid then
    begin
      Q := Dx mod GridSize;
      R := Dy mod GridSize;
    end;

    for i := 0 to TargetCount - 1 do
      TTargetObj(fTargetList[i]).SizeFocus(dx - q, dy - r, fCapturedBtnPos);
    if assigned(fDuringEvent) then
      fDuringEvent(self as TObject, dx - q, dy - r, fState);
  end
  else
  begin

    if (not KeyIsPressed(Self.FDGKey)) and self.fAlignToGrid then
    begin
      Q := Dx mod GridSize;
      R := Dy mod GridSize;
    end;

    for i := 0 to TargetCount - 1 do
      TTargetObj(fTargetList[i]).MoveFocus(dx - q, dy - r);
    if assigned(fDuringEvent) then
      fDuringEvent(self, dx - q, dy - r, fState);
  end;
  DrawRect;
end;

//------------------------------------------------------------------------------

procedure TSizeCtrl.DoMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState);
var
  i
  {$IFDEF FPC}
  :integer;
  {$ELSE}
  ,k: integer;
  list: TList;
  {$ENDIF}
begin
  if fState = scsReady then
    exit;
  {$IFNDEF FPC}
  ClipCursor(nil);
  {$ENDIF}
  THackedControl(fCapturedCtrl).MouseCapture := False;
  {$IFDEF FPC}
  THackedControl(fCapturedCtrl).SetCursor(crHandPoint);
  {$ENDIF}
  fCapturedCtrl := nil;
  if not fEscCancelled then
    for i := 0 to TargetCount - 1 do
    begin
      TTargetObj(fTargetList[i]).EndFocus;
      {$IFnDEF FPC}
      TTargetObj(fTargetList[i]).fPanelsNames.Clear;
      list := TTargetObj(fTargetList[i]).fPanels;
      for k := 0 to list.Count - 1 do
        TObject(list[k]).Free();
      list.Clear;
      {$ENDIF}
    end;

  fEscCancelled := False;

  if assigned(fEndEvent) then
    fEndEvent(self, fState);

  fState := scsReady;
end;

//------------------------------------------------------------------------------

procedure TSizeCtrl.Hide;
begin
  Enabled := False;
end;

//------------------------------------------------------------------------------

procedure TSizeCtrl.Show;
begin
  Enabled := True;
end;

//------------------------------------------------------------------------------

procedure TSizeCtrl.UpdateBtnCursors;
var
  i: integer;
  j: TBtnPos;
begin

  if fMultiResize or (TargetCount = 1) then
  begin
    fValidBtns := [bpLeft, bpTopLeft, bpTop, bpTopRight, bpRight,
      bpBottomRight, bpBottom, bpBottomLeft];
    for i := 0 to TargetCount - 1 do
      case TTargetObj(fTargetList[i]).fTarget.Align of
        alTop: fValidBtns := fValidBtns - [bpLeft, bpTopLeft, bpTop,
            bpTopRight, bpRight, bpBottomRight, bpBottomLeft];
        alBottom: fValidBtns :=
            fValidBtns - [bpLeft, bpTopLeft, bpTopRight, bpRight,
            bpBottomRight, bpBottom, bpBottomLeft];
        alLeft: fValidBtns := fValidBtns - [bpLeft, bpTopLeft, bpTop,
            bpTopRight, bpBottomRight, bpBottom, bpBottomLeft];
        alRight: fValidBtns :=
            fValidBtns - [bpTopLeft, bpTop, bpTopRight, bpRight,
            bpBottomRight, bpBottom, bpBottomLeft];
        alClient: fValidBtns := [];
        {$IFDEF VER3U}
        alCustom: fValidBtns := [];
        {$ENDIF}
      end;
  end
  else
    fValidBtns := [];

  for i := 0 to TargetCount - 1 do
    with TTargetObj(fTargetList[i]) do
      for j := bpLeft to LastBtn do
        fBtns[j].UpdateBtnCursorAndColor;
end;

//------------------------------------------------------------------------------

procedure TSizeCtrl.SetMoveOnly(Value: boolean);
begin
  if fMoveOnly = Value then
    exit;
  fMoveOnly := Value;
  UpdateBtnCursors;
end;

//------------------------------------------------------------------------------

function TSizeCtrl.IsValidSizeBtn(BtnPos: TBtnPos): boolean;
begin
  Result := (TargetCount > 0) and (TTargetObj(fTargetList[0]).fBtns[BtnPos].Cursor <>
    crDefault);
end;

//------------------------------------------------------------------------------

function TSizeCtrl.IsValidMove: boolean;
var
  i: integer;
begin
  Result := False;
  for i := 0 to TargetCount - 1 do
    if (TTargetObj(fTargetList[i]).fTarget.Align <> alNone) then
      exit;
  Result := True;
end;

//------------------------------------------------------------------------------

procedure TSizeCtrl.SetMultiResize(Value: boolean);
begin
  if Value = fMultiResize then
    exit;
  fMultiResize := Value;
  UpdateBtnCursors;
end;

//------------------------------------------------------------------------------

procedure TSizeCtrl.SetEnabledBtnColor(aColor: TColor);
begin
  if fEnabledBtnColor = aColor then
    exit;
  {$IFDEF FPC}
  if aColor = clBlack then inc(AColor,1);
  {$ENDIF}
  fEnabledBtnColor := aColor;
  UpdateBtnCursors;
end;

//------------------------------------------------------------------------------

procedure TSizeCtrl.SetHoverBtnColor(aColor: TColor);
begin
  if fHoverBtnColor = aColor then
    exit;
  {$IFDEF FPC}
  if aColor = clBlack then inc(AColor,1);
  {$ENDIF}
  fHoverBtnColor := aColor;
  UpdateBtnCursors;
end;

//------------------------------------------------------------------------------

procedure TSizeCtrl.SetHoverBtnFrameColor(aColor: TColor);
begin
  if fHoverBtnFrameColor = aColor then
    exit;
  {$IFDEF FPC}
  if aColor = clBlack then inc(AColor,1);
  {$ENDIF}
  fHoverBtnFrameColor := aColor;
  UpdateBtnCursors;
end;

//------------------------------------------------------------------------------

procedure TSizeCtrl.SetDisabledBtnColor(aColor: TColor);
begin
  if fDisabledBtnColor = aColor then
    exit;
  {$IFDEF FPC}
  if aColor = clBlack then inc(AColor,1);
  {$ENDIF}
  fDisabledBtnColor := aColor;
  UpdateBtnCursors;
end;

//------------------------------------------------------------------------------

procedure TSizeCtrl.HardReset(sizes:boolean=false);
var i: integer;
j: TBtnPos;
begin
if TargetCount > 0 then
 for i := 0 to TargetCount - 1 do
    with TTargetObj(fTargetList[i]) do
    begin
      for j := bpLeft to LastBtn do
        fBtns[j].Reset;
      if sizes then
        Update;
    end;
end;

//------------------------------------------------------------------------------

procedure TSizeCtrl.UpdateGrid;
begin
  if Assigned(TWinControl(Owner)) then
  begin
     {$IFDEF FPC}
     if TWinControl(Owner).Visible then
     {$ENDIF}
        TWinControl(Owner).Repaint;
     {$IFNDEF FPC}
     if not (fForm is TCustomForm) then
     begin
       if Assigned(fGridForm) then
       begin
          fGridForm.Visible := True;
          fGridForm.Repaint;
       end
       else
        CreateGrid;
     end;
     {$ELSE}
     fForm.Repaint;
     {$ENDIF}
  end;

end;

//------------------------------------------------------------------------------

procedure TSizeCtrl.setGridSize(Value: integer);
begin
  if (fGridSize <> Value) and (1 <= Value ) and (Value <= 50)
  then
  begin
    fGridSize := Value;
    UpdateGrid;
  end;
end;

//------------------------------------------------------------------------------

procedure TSizeCtrl.setGridWhite(Value: TColor);
begin
  if fGridWhite = Value then Exit;
  fGridWhite := Value;
  UpdateGrid;
end;

//------------------------------------------------------------------------------

procedure TSizeCtrl.setGridBlack(Value: TColor);
begin
  if fGridBlack = Value then Exit;
  fGridBlack := Value;
  UpdateGrid;
end;

//------------------------------------------------------------------------------

procedure TSizeCtrl.SetBtnCount(Value: TSizeCtrlBtnCount);
var i: integer;
begin
  if fBtnCount <> Value then
  begin
    fBtnCount := Value;
    fLastBtn := TBtnPos( (integer(fBtnCount) + 1) * 4 );
    if TargetCount > 0 then
     for i := 0 to TargetCount - 1 do
      with TTargetObj(fTargetList[i]) do
        ReconfButtons;
  end;
end;

//------------------------------------------------------------------------------

procedure TSizeCtrl.SetBtnSize(Value: integer);
begin
  if (fBtnSize <> Value) and (3 <= Value) and (Value <= 50) then
  begin
    fBtnSize := Value;
    HardReset(true);
  end;
end;

//------------------------------------------------------------------------------

procedure TSizeCtrl.SetBtnAlphaBlend(Value: integer);
begin
  if (fBtnAlpha <> Value) and (0 <= Value) and (Value <= 255) then
  begin
    fBtnAlpha := Value;
    HardReset;
  end;
end;

//------------------------------------------------------------------------------

procedure TSizeCtrl.setMovePanelAlphaBlend(Value: integer);
begin
  if (fMovePanelAlpha <> Value) and (0 <= Value) and (Value <= 255) then
    fMovePanelAlpha := Value;
end;

//------------------------------------------------------------------------------

procedure TSizeCtrl.setPanelImage(Value: TPicture);
begin
  if  Value = fPanelImage then
  Exit;
  fPanelImage.Assign( Value );
end;

//------------------------------------------------------------------------------

procedure TSizeCtrl.SetBtnImage(Value: TPicture);
begin
  if  Value = fBtnImage then
  Exit;
  fBtnImage.Assign( Value );
  HardReset;
end;

//------------------------------------------------------------------------------

procedure TSizeCtrl.SetHoverBtnImage(Value: TPicture);
begin
  if  Value = fHoverBtnImage then
  Exit;
  fHoverBtnImage.Assign( Value );
  HardReset;
end;

//------------------------------------------------------------------------------

procedure TSizeCtrl.SetDisabledBtnImage(Value: TPicture);
begin
  if  Value = fDisabledBtnImage then
  Exit;
  fDisabledBtnImage.Assign( Value );
  HardReset;
end;

//------------------------------------------------------------------------------

procedure TSizeCtrl.SetStretchBtnImage(Value: boolean);
begin
  if  Value = fStretchBtnImage then
  Exit;
  fStretchBtnImage := Value;
  HardReset;
end;

//------------------------------------------------------------------------------

procedure TSizeCtrl.SetBtnShape(v: TSizeBtnShapeType);

begin
  if fBtnShape = v then
  exit;
   fBtnShape := v;
  HardReset;
end;

//------------------------------------------------------------------------------

procedure TSizeCtrl.SetBtnFrameColor(aColor: TColor);
begin
  if fBtnFrameColor = aColor then
  exit;
  {$IFDEF FPC}
  if aColor = clBlack then inc(AColor,1);
  {$ENDIF}
   fBtnFrameColor := aColor;
  UpdateBtnCursors;
end;

//------------------------------------------------------------------------------

procedure TSizeCtrl.SetDisabledBtnFrameColor(aColor: TColor);
begin
  if fDisabledBtnFrameColor = aColor then
  exit;
  {$IFDEF FPC}
  if aColor = clBlack then inc(AColor,1);
  {$ENDIF}
   fDisabledBtnFrameColor := aColor;
  UpdateBtnCursors;
end;

//------------------------------------------------------------------------------

procedure TSizeCtrl.SetPopupMenu(Value: TPopupMenu);
begin
  fPopupMenu := Value;
  if Value = nil then
    exit;
  Value.FreeNotification(Self);
end;

//------------------------------------------------------------------------------

procedure TSizeCtrl.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = PopupMenu) then
    PopupMenu := nil;
end;

//------------------------------------------------------------------------------

procedure TSizeCtrl.DoPopupMenuStuff;
var
  Handled: boolean;
  {$IFDEF FPC}
  {%H-}pt: TPoint;
  {$ELSE}
  pt:    TPoint;
  {$ENDIF}
  targetCtrl: TControl;
begin
  if not assigned(fPopupMenu) then
    exit;
  {$IFDEF FPC}
  GetCursorPos({%H-}pt);
  {$ELSE}
  GetCursorPos(pt);
  {$ENDIF}
  targetCtrl := TargetCtrlFromPt(pt);
  if not assigned(targetCtrl) then
    exit;
  Handled := False;
  if Assigned(FOnContextPopup) then
    fOnContextPopup(Self, pt, Handled);
  if Handled then
    exit;
  THackedControl(owner).SendCancelMode(nil);
  fPopupMenu.PopupComponent := targetCtrl;
  PopupMenu.Popup(Pt.X, Pt.Y);
end;

//------------------------------------------------------------------------------

procedure TSizeCtrl.formPaint(Sender: TObject);
var
  i, j, w, h: integer;
  c, g: TColor;
  {$IFNDEF FPC}
  obj: TWinControl;
  {$ENDIF}
begin
  if not Assigned(Self) or
     not Assigned(fForm)or
     not Enabled then
    exit;
  {$IFDEF FPC}
  DrawRect;
  {$ENDIF}
  if not ShowGrid then
    exit;
  if GridSize < 5 then
    exit;

  w := TControl(fForm).Width;
  h := TControl(fForm).Height;
  {$IFDEF FPC}
  c := TCustomControl(fForm).Color;
  {$ELSE}
  if fForm is TForm then
    c := TForm(fForm).Color
  else
  begin
  obj := TWinControl(Sender);
  while not (obj is TCustomForm) and Assigned(obj) do
      obj := obj.parent;
    if obj is TCustomForm then
      begin
        c := TCustomForm(obj).Color;
        TForm(Sender).Top := 0;
        TForm(Sender).Left := 0;
        TForm(Sender).Width := TWinControl(fForm).Width;
        TForm(Sender).Height := TWinControl(fForm).Height;
      end;
  end;
  {$ENDIF}

  lastW := w;
  lastH := h;
  lastColor := c;
  {$IFDEF FPC}
  g := c shr 8;
  {$ELSE}
  g := GetGValue(c);

  if fGrid = nil then
    fGrid := TBitmap.Create;

  fGrid.Width := w;
  fGrid.Height := h;
  fGrid.Canvas.Brush.Color := c;
  fGrid.Canvas.Pen.Style := psClear;
  fGrid.Canvas.Rectangle(0, 0, w + 1, h + 1);
  {$ENDIF}
  for i := 0 to w div GridSize do
    for j := 0 to h div GridSize do
    begin
      {$IFDEF FPC}
      if g < 180 then
        TCustomControl(Sender).Canvas.DrawPixel(I * GridSize, J * GridSize,
        Graphics.TColorToFPColor(
        fGridWhite))
      else
        TCustomControl(Sender).Canvas.DrawPixel(I * GridSize, J * GridSize,
        Graphics.TColorToFPColor(fGridBlack));
    end;
      {$ELSE}
      if g < 180 then
        fGrid.Canvas.Pixels[I * GridSize, J * GridSize] := fGridWhite
      else
        fGrid.Canvas.Pixels[I * GridSize, J * GridSize] := fGridBlack;

    end;
    TForm(Sender).Canvas.Draw(0, 0, fGrid);
    {$ENDIF}
end;

//------------------------------------------------------------------------------

procedure TSizeCtrl.SetShowGrid(const Value: boolean);
begin
  if FShowGrid = Value then Exit;
  FShowGrid := Value;
  UpdateGrid;
end;

//------------------------------------------------------------------------------

function TSizeCtrl.getSelected: TList;
var
  i: integer;
begin
  Result := TList.Create;
  for i := 0 to fTargetList.Count - 1 do
  begin
    Result.Add(TTargetObj(fTargetList[i]).fTarget);
  end;
end;

//------------------------------------------------------------------------------

procedure TSizeCtrl.toBack(CNTR: TControl);
var
  i, x: integer;
  res: TRegisteredObj;
  newList: TList;
begin
  newList := TList.Create;
  for i := 0 to fRegList.Count - 1 do
  begin
    res := TRegisteredObj(fRegList[i]);
    if (integer(res.fControl) = integer(CNTR)) then
    begin
      x := i;
      break;
    end;
  end;

  for i := 0 to fRegList.Count - 1 do
  begin
    if i = x then
      continue;
    newList.Add(fRegList[i]);
  end;


  newList.Add(fRegList[x]);

  fRegList.Free;
  fRegList := newList;
end;


//------------------------------------------------------------------------------

procedure TSizeCtrl.toFront(CNTR: TControl);
var
  i, x: integer;
  res: TRegisteredObj;
  newList: TList;
begin
  newList := TList.Create;
  for i := fRegList.Count - 1 downto 0 do
  begin
    res := TRegisteredObj(fRegList[i]);
    if (integer(res.fControl) = integer(CNTR)) then
    begin
      x := i;
    end;
  end;

  for i := fRegList.Count - 1 downto 0 do
  begin
    if i = x then
      continue;
    newList.Add(fRegList[i]);
  end;

  newList.Add(fRegList[x]);

  fRegList.Free;
  fRegList := newList;
end;
{$IFNDEF FPC}
{ TMovePanel }

constructor TMovePanel.Create(AOwner: TComponent;AsizeCtrl:TSizeCtrl);
begin
  fSizeCtrl := ASizeCtrl;
  inherited CreateNew(nil);
  Loaded;

  BorderStyle := bsNone;
  AutoSize := False;
  Position := poDesigned;

  FormStyle := fsStayOnTop;
  Visible := False;
  Parent := TWinControl(AOwner);
  OnPaint := DoPaint;
  OnResize := SetBoundsRect;
end;

//------------------------------------------------------------------------------

procedure TMovePanel.SetBoundsRect;
begin
  invalidate;
end;

//------------------------------------------------------------------------------

procedure TMovePanel.setfcanvas(fCanvas: TCanvas);
begin
  Canvas.Pen.Assign(fCanvas.Pen);
  Canvas.Brush.Assign(fCanvas.Brush);
  Color := fCanvas.Brush.Color;
end;

//------------------------------------------------------------------------------
procedure TMovePanel.DoPaint(Sender: TObject);
begin
  setfcanvas(fSizeCtrl.movePanelCanvas);
  {$IFDEF VER3UP}
    if Canvas.Brush.Style = bsClear then
  begin
    TransparentColor := True;
    TransparentColorValue := Color;
  end
  else
  begin
    TransparentColor := False;
  end;
  AlphaBlend := fSizeCtrl.MovePanelAlphaBlend < 255;
  AlphaBlendValue := fSizeCtrl.MovePanelAlphaBlend;
  {$ELSE}
   if Canvas.Brush.Style = bsClear then
      Color := fSizeCtrl.fParentForm.Color;
  {$ENDIF}
  If Assigned(fSizeCtrl.MovePanelImage.Graphic)
  and not(fSizeCtrl.MovePanelImage.Graphic.Empty) then
  begin
     if fSizeCtrl.StretchMovePanelImage then
      Canvas.StretchDraw(Rect(0,0,Width,Height), fSizeCtrl.MovePanelImage.Graphic)
     Else
     Begin
      Canvas.Draw(0,0,fSizeCtrl.MovePanelImage.Graphic);
     end;
  End
  Else
      Canvas.Rectangle(0, 0, Width, Height);
end;
{$ENDIF}
end.
