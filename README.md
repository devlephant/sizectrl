## Description
TSizeCtrl is a non-visual V+LCL component, which allows you to resize, select and move TControl decadents

Unlike original realization, current version supports both Delphi and Lazarus, so it's inspired by original version by Angus Johnson, but, now it's not the same as legacy/old version :)

##### Features:
<ul> <li>Fully customizable buttons: you can change count (from 4 to 8), colour, size, transparency, shape, and if you want to - you can set the button image</li>
<li>5 shape types: Square, Circle, Rhombus, Triangle and RoundRect</li>
<li> Customizable selection frame: movePanelCanvas property to access all of the selection frames canvases at once, with transparency (Brush.Style := bsClear) support</li>
<li> ShowFrame property to show/hide selection frame</li>
<li> MovePanelAlphaBlend property to specify alpha-channel of the sizing/selection frame </li>
<li> MovePanelImage property to set the Selection Frame image </li>
<li> TCustomForms support: when form is resized with TSizeCtrl, it cannot be closed or maximized/minimized </li>
<li>Grid support, with properties: gridSize, showGrid, gridColor, gridColorContrast </li></ul>
<ul><li> Resizing options:</li><ul>
<li> AlignToGrid property - align controls to the grid
<li> ApplySizes property - activate to change object's size immediately
<li> MultiTargetResize property - to deny/allow resizing of few targets at once
<li> Constraints property to specify minimal and maximal sizes of the resizable object
<li> MoveOnly property to deny/allow resizing of an objects
<li> Objects tag support, to deny object from selecting
<li> Customizable buttons: colours ({content-fill, border}) and images for Enabled, Hovered and Disabled states 
<li> ApplySizes property - to apply sizes to the object immediately
<li> EditDisabled property to allow (Enabled := False)->Disabled components editing
<li> Different resize-during button styles: none (buttons are hidden),  buttons (all buttons are visible), line, single button
<li> Different resize-during button hide options: none (buttons are visible, but static), hide (buttons are hidden), move (buttons will be moved in case of moving, but hidden in case of sizing) </li>
	</ul></ul>
<ul><li> Additional options:</li><ul>
<li> Stretch properties supplied with every Image(TPicture) properties, to specify which images you would like to store in the original size
<li> Customizable modification keys (selection, moving/sizing, align-ignoring keys)
<li> OnHover events for TSizeBtns (sizing grips/handles) </li>
	</ul></ul>

##### Limitations
<ul> Lazarus: <ul>
<li> Currently not ssupporting MovePanelImage property
<li> In some platforms it cannot support Alpha-Transparency
</ul> </ul>
<ul> FireMonkey: <ul>
<li> Not tested yet
</ul> </ul>

##### Errors and bugs:
<ul><li> Fixed common bugs:</li><ul>
<li> Bug with Sizing Grid (ShowGrid) on non-TForm controls
<li> Selection bug with Form focus (when you trying to change the form, and TSizeCtrl still handles events)
<li> Selection bug with Forms (when you size form, its Caption Bar with action buttons is still available, which is kinda irritating)
<li> Moving bugs with DoubleBuffering Enabled
<li> Bug with btn size, when object Left+Top = Btn Diameter(Size)
<li> Bug with recursion in DefWindowProc -- FormWindowProc
<li> Bug with unassigned control, which caused by size-during component selection
<li> Bug with cursor: sometimes it's won't change, when the moving is started (with TShape, for example)
<li> Moving object with keys, while editing it by mouse, causes sizing bugs
<li> Bug with invisible objects: it denies visible, but upper object selection</li>
</ul></ul>
<ul><li> Known modern bugs:</li><ul>
<li> 1. Add disabled objects selection support [Lazarus]
		<br>Possible solution: hook all WndProces recursive?</li>
</ul></ul>

##### Nearly plans:
<ul><li> New Features:</li><ul>
<li> 1. Align with lines (function, ef)
<li> 2. Detect control polygon to draw shape to fit the form
<li> 3. Custom tag(s) support 
<li> 4. D/L CL real transparency fast support with DrawFocusRect method and Focus rect type, which can specify how to draw the focus rect
<li> 5. Property to hide focus rect only when resizing </li>
  </ul></ul>
<ul><li> Refactorings:</li><ul>
<li>1. Flipping control, when its sizes is too small
  (Changing position + resizing)
<li>2. Regenerate Frame topmost settings(Add FrameOnTop property, e.g make the TMovePanel parent the same as TSizeBtn parent, if needed) [Delphi,LAzarus]</li>
<li>3. Limit resizing when control is inserted in aligned or when control contains both aligned and unaligned sub-ctrls (childs)
  </ul></ul>
