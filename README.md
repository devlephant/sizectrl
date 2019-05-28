# sizectrl
Updated TSizeCtrl (v8), which originally performed by Angus Johnson, so... it's inspired by original, but, now it's not the same as legacy/old version :)

## Description
TSizeCtrl is a non-visual VCL component, which allows you to resize, select and move TControl decadents

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
<li> ApplySizes property - activate to change object's size immediately
<li> MultiTargetResize property - to deny/allow resizing of few targets at once
<li> Constraints property to specify minimal and maximal sizes of the resizable object
<li> MoveOnly property to deny/allow resizing of an objects
<li> Objects tag support, to deny object from selecting
<li> Customizable buttons: colours ({content-fill, border}) and images for Enabled, Hovered and Disabled states 
<li> Different resize-during button styles: none (buttons are hidden),  buttons (all buttons are visible), line, single button
<li> Different resize-during button hide options: none (buttons are visible, but static), hide (buttons are hidden), move (buttons will be moved in case of moving, but hidden in case of sizing)</ul></ul>
<ul><li> Additional options:</li><ul>
<li> Stretch properties supplied with every Image(TPicture) properties, to specify which images you would like to store in the original size </li>
<li> Customizable modification keys (selection, align-ignoring keys)
<li> OnHover events for TSizeBtns (sizing grips/handles) </li>
<li> ApplySizes property - to apply sizes to the object immediately </li></ul></ul>

##### Fixed bugs:
<ul><li> Fixed common bugs:</li><ul>
<li> Bug with Sizing Grid (ShowGrid) on non-TForm controls
<li> Selection bug with Form focus (when you trying to change the form, and TSizeCtrl still handles events)
<li> Selection bug with Forms (when you size form, its Caption Bar with action buttons is still available, which is kinda irritating)
<li> Moving bugs with DOubleBuffering Enabled</li>
<li> Bug with btn size, when object Left+Top = Btn Diameter(Size)</li></ul></ul>

<ul><li> Known modern bugs:</li><ul>
<li> {Nothing} </li>
</ul> </ul>

##### Nearly plans:
<ul><li> New Features:</li><ul>
<li> 1. Selection of disabled objects (cursrtom property)
<li> 2. Align with lines (function, ef)
<li> 3. Detect control polygon to draw shape to fit the form
<li> 4. Add hotkey + arrow keys controlling to sizing/moving procedures
  </ul></ul>
<ul><li> Refactorings:</li><ul>
<li>1. Flipping control, when its sizes is too small
  (Changing position + resizing)</li>
<li>2. Regenerate Frame topmost settings(Add FrameOnTop property, e.g make the TMovePanel parent the same as TSizeBtn parent, if needed)
  </ul></ul>
