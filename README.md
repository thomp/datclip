# datclip

*Show the content of the primary selection and the clipboard in an emacs buffer.*

---

**datclip** allows the Emacs user to bypass some of the aggravation which can occur with selections by showing them in a dedicated buffer.

## Installation

Save datclip.el somewhere. Add a line to your ~/.emacs file which directs emacs to load datclip.el:

```
(load "/some/path/to/datclip.el")
```

## Use

The default binding to show selection and clipboard content is <kbd>s-c</kbd> (the 'Super' key and the 'c' key).

Press <kbd>p</kbd> to grab content of primary (push it onto the kill ring).

Press <kbd>q</kbd> when you're done working with the datclip buffer.

## License

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

If you did not receive a copy of the GNU General Public License along with this program, see http://www.gnu.org/licenses/.
