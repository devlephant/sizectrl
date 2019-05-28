# sizectrl
Updated TSizeCtrl (v8), which originally performed by Angus Johnson, so... it's inspired by original, but, now it's not the same as legacy/old version :)

## Description
TSizeCtrl is a non-visual VCL component, which allows you to resize, select and move TControl decadents

##### Features:
<ul> Fully customizable buttons: you can change count (from 4 to 8), colour, size, transparency, shape, and if you want to - you can set button image
<li>5 shape types: Square, Circle, Rhombus, Triangle and RoundRect</li></ul>
<ul> Customizable selection frame: movePanelCanvas property to access all selection frame's canvases at once, with transparency (Brush.Style := bsClear) support</ul>
<ul> ShowFrame property to show/hide selection frame</ul>
<ul> MovePanelAlphaBlend property to specify alpha-channel of the sizing/selection frame </ul>
<ul> MovePanelImage property to set Selection Frame image </ul>
<ul> TCustomForms support: when form is resized with TSizeCtrl, it cannot be closed or maximized/minimized </ul>
<ul>Grid support, with properties: gridSize, showGrid, gridColor, gridColorContrast </ul>
<ul> Resizing options:<ul>
<li> ApplySizes property - activate to change object's size immediately
<li> MultiTargetResize property - to deny/allow resizing of few targets at once
<li> Constraints property to specify minimal and maximal sizes of the resizable object
<li> MoveOnly property to deny/allow resizing of an objects
<li> Objects tag support, to deny object from selecting
<li> Customizable buttons: colours ({content-fill, border}) and images for Enabled, Hovered and Disabled states 
<li> Different resize-during button styles: none (buttons are hidden),  buttons (all buttons are visible), line, single button
<li> Different resize-during button hide options: none (buttons are visible, but static), hide (buttons are hidden), move (buttons will be moved in case of moving, but hidden in case of sizing)</ul></ul>
<ul> Additional options:<ul>
<li> Stretch properties supplied with every Image(TPicture) properties, to specify which images you would like to store in the original size </li>
<li> Customizable modification keys (selection, align-ignoring keys)
<li> OnHover events for TSizeBtns (sizing grips/handles) </li>
<li> ApplySizes property - to apply sizes to the object immediately </li></ul></ul>

##### Fixed bugs:
<ul> Fixed common bugs:<ul>
<li> Bug with Sizing Grid (ShowGrid) on non-TForm controls
<li> Selection bug with Form focus (when you trying to change the form, and TSizeCtrl still handles events)
<li> Selection bug with Forms (when you size form, its Caption Bar with action buttons is still available, which is kinda irritating)
<li> Moving bugs with DOubleBuffering Enabled</li></ul></ul>

<ul> Known modern bugs:<ul>
<li> Bug with BtnSize, when the value can be easily divided by 2 </li>
</ul> </ul>
