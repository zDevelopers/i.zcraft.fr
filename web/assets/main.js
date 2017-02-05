'use strict';

document.addEventListener("DOMContentLoaded", function () {

    document.body.className = "preload js";
    window.addEventListener("load", function () {
        document.body.className = "js";
    });

    (function setupFileUpload()
    {
        var width = window.innerWidth || document.documentElement.clientWidth || document.body.clientWidth;
        var fileLabel = document.getElementById("file");
        var fileInput = document.querySelector('#file input[type="file"]');
        var submitInput = document.querySelector('input[type="submit"]');
        var selectParagraph = document.getElementById("select_p");
        var deletionSelect = document.getElementById("expires_after");
        var deletionCheckbox = document.getElementById("expires");
        var buttonFieldset = document.createElement("fieldset");
        var optionsButton = document.createElement("button");
        
        if(!(fileLabel && fileInput && selectParagraph && submitInput && deletionSelect && deletionCheckbox)) return;

        var text_second_invite = '<em>Glissez-déposez</em> ou <em>cliquez</em> dans la zone pour sélectionner une autre image.';
        if (width < 721) {
            var rand = Math.random();
            if (rand < 0.9) {
                selectParagraph.innerHTML = '<em>Tapotez</em> pour sélectionner une image';
                text_second_invite = '<em>Tapotez</em> dans la zone pour sélectionner une autre image.';
            }
            else if (rand < 0.995) {
                selectParagraph.innerHTML = '<em>Caressez tendrement</em> pour sélectionner une image';
                text_second_invite = '<em>Caressez la zone tendrement</em> pour sélectionner une autre image.';
            }
            else {
                selectParagraph.innerHTML = '<em>Léchez avec passion</em> pour sélectionner une image';
                text_second_invite = '<em>Léchez la zone avec passion</em> pour sélectionner une autre image.';
            }
        }

        var infoParagraph = makeInfoParagraph();
        var validFileType = false;
        var validFileSize = false;
        var dragStatus = 0;

        fileInput.addEventListener("change", updateForm);
        submitInput.parentNode.replaceChild(buttonFieldset, submitInput);
        optionsButton.textContent = "🔧";
        optionsButton.id = "optionsButton";
        optionsButton.addEventListener("click", function (e) {
            getDrawer("options").open = true;
            Dropdown.open();
            e.preventDefault();
            return false;
        });
        buttonFieldset.appendChild(optionsButton);
        buttonFieldset.appendChild(submitInput);
        buttonFieldset.id = "buttons";

        updateForm();

        function preventDrop(e)
        {
            if(e.target === fileInput) return;
            e.dataTransfer.dropEffect = 'none';
        }
        function dragStart(e)
        {
            ++dragStatus;
            document.body.classList.add('dragging');
        }

        function dragStop(e)
        {
            --dragStatus;
            if(dragStatus == 0) {
                document.body.classList.remove('dragging');
            }
        }

        document.documentElement.addEventListener("dragenter", preventDrop);
        document.documentElement.addEventListener("dragover", preventDrop);

        document.documentElement.addEventListener("dragenter", dragStart);
        document.documentElement.addEventListener("dragleave", dragStop);
        fileInput.addEventListener("change", dragStop);

        deletionSelect.addEventListener("change", function (e) {
            deletionCheckbox.checked = true;
        });

        function updateForm()
        {
            if(fileInput.files && fileInput.files[0]) {
                if(selectParagraph.parentNode) {
                    selectParagraph.parentNode.removeChild(selectParagraph);
                }
                if(!infoParagraph.parentNode) {
                    fileLabel.appendChild(infoParagraph);
                }
            }
            infoParagraph.updateInfo();
        }

        function makeInfoParagraph()
        {
            var infoParagraph = document.createElement("p");

            var previewContainer = infoParagraph.appendChild(document.createElement("figure"));
            var previewImage = previewContainer.appendChild(document.createElement("img"));
            var infoList = infoParagraph.appendChild(document.createElement("ul"));
            var fileNameInfo = infoList.appendChild(document.createElement("li"));
            var fileTypeInfo = infoList.appendChild(document.createElement("li"));
            var fileSizeInfo = infoList.appendChild(document.createElement("li"));
            var selectOtherParagraph = infoParagraph.appendChild(document.createElement("p"));
            selectOtherParagraph.innerHTML = text_second_invite;

            infoParagraph.id = "info_p";
            infoParagraph.updateInfo = function () {
                fileNameInfo.innerHTML = "";
                fileNameInfo.appendChild(document.createTextNode(fileInput.value));
                if(fileInput.files && fileInput.files[0]) {
                    var selectedFile = fileInput.files[0];
                    var maxFileSize = parseInt(fileInput.dataset.maxsize);

                    validFileSize = selectedFile.size <= maxFileSize;
                    validFileType = selectedFile.type.substr(0, 6) == "image/";

                    fileTypeInfo.innerHTML = "Fichier <code>" + (selectedFile.type || "Inconnu");
                    fileSizeInfo.innerHTML = beautifySize(selectedFile.size);

                    fileTypeInfo.className = validFileType ? "" : "invalid";
                    fileSizeInfo.className = validFileSize ? "" : "invalid";
                    submitInput.className = (validFileSize && validFileType) ? "" : "invalid";
                    infoParagraph.className = (validFileSize && validFileType) ? "" : "invalid";
                    submitInput.disabled = !(validFileSize && validFileType);

                    if(validFileType) {
                        var reader = new FileReader();
                        reader.onload = function (e) {
                            previewImage.src = e.target.result;};
                        reader.readAsDataURL(selectedFile);
                    } else {
                        previewImage.removeAttribute("src");
                    }

                    if(!validFileType) {submitInput.value = "Fichier invalide";}
                    else if(!validFileSize) {submitInput.value = "Fichier trop volumineux";}
                    else {submitInput.value = "Envoyer";}
                } else {
                    submitInput.disabled = true;
                    submitInput.value = "Sélectionnez un fichier";
                }

            };
            return infoParagraph;
        }

        function beautifySize(size)
        {
            var units = ['octets', 'Kio', 'Mio', 'Gio', 'Tio'];
            var currentUnit = 0;

            while (size > 1024) {
                ++currentUnit;
                size /= 1024;
            }

            return size.toFixed(1) + ' ' + units[currentUnit];
        }
    })();

    (function setupClipboardActions()
    {
        function selectText(element)
        {
            var doc = document, range, selection;

            if (doc.body.createTextRange) {
                range = document.body.createTextRange();
                range.moveToElementText(element);
                range.select();
            } else if (window.getSelection) {
                selection = window.getSelection();
                range = document.createRange();
                range.selectNodeContents(element);
                selection.removeAllRanges();
                selection.addRange(range);
            }
        }

        var linkblocks = document.getElementsByClassName("linkblock");
        for(var i = 0; i < linkblocks.length; ++i) {
            var linkblock = linkblocks[i];
            var button = linkblocks[i].appendChild(document.createElement("button"));
            button.textContent = "📋";
            button.title = "Copier dans le presse-papiers";

            button.addEventListener("click", function (button) {
                var copyBuffer = document.body.appendChild(document.createElement("textarea"));
                var button = button.target;
                copyBuffer.className = "copy-buffer";
                copyBuffer.value = button.previousElementSibling.textContent;
                copyBuffer.select();
                try
                {
                    if(!document.execCommand("copy")) {
                        console.error("Copy failed.");
                        button.setAttribute("data-tooltip", "Erreur lors de la copie");
                    }
                    else {
                        button.setAttribute("data-tooltip", "Copié !");
                    }
                }
                catch(e)
                {
                    console.error("Error while executing copy command: %o", e);
                    button.setAttribute("data-tooltip", "Erreur lors de la copie");
                }

                button.className = "tooltip-visible";
                if(button.timeoutId) clearTimeout(button.timeoutId);
                button.timeoutId = setTimeout(function () {
                    button.className = "";
                }, 2000);

                document.body.removeChild(copyBuffer);
            });


            linkblock.firstElementChild.addEventListener("click", function (e) {
                selectText(e.target);
            });
        }
    })();


    var drawers = {};

    var Dropdown = {
        id: "main-dropdown",
        timeout: 2000,
        element: document.createElement("div"),
        _timeoutId: null,

        init: function () {
            Dropdown.element.id = Dropdown.id;
            Dropdown.element.addEventListener("click", closeAll);
        },

        open: function () {
            document.body.appendChild(Dropdown.element);
            setTimeout(function () {
                Dropdown.element.className = "visible";
            });

            if(Dropdown._timeoutId)
                clearTimeout(Dropdown._timeoutId);
        },

        close: function () {
            Dropdown.element.className = "";
            Dropdown._timeoutId = setTimeout(Dropdown._onAnimationEnd, Dropdown.timeout);
        },

        _onAnimationEnd: function () {
            document.body.removeChild(Dropdown.element);
            Dropdown._timeoutId = null;
        }
    };

    var NavigationDrawer = {
        id: '',

        get element()
        {
            return document.getElementById(this.id)
                || console.error("Invalid drawer id: %s", this.id);
        },

        new: function (drawer_id) {
            var obj = Object.create(this);
            obj.id = drawer_id;
            return obj;
        },

        get open()
        {
            return this.element.className.split(" ").indexOf("visible") >= 0;
        },

        set open(isOpen)
        {
            this.element.className = isOpen ? "visible" : "";
        }
    };

    function getDrawer(drawer_id)
    {
        var drawer = drawers[drawer_id];

        if(!drawer)
        {
            drawer = NavigationDrawer.new(drawer_id);
            drawers[drawer_id] = drawer;
        }

        return drawer;
    }

    function openDrawer()
    {
        getDrawer(this.getAttribute("data-drawer")).open = true;
        Dropdown.open();
    }

    function closeAll()
    {
        for(var drawer_id in drawers)
        {
            drawers[drawer_id].open = false;
        }
        Dropdown.close();
    }

    Dropdown.init();
});


/**
 * Saves an image into the browser local storage.
 *
 * @param name The image name.
 * @param storage_name The image' storage name.
 * @param url The image's URL.
 * @param url_mini The image's thumbnail URL.
 * @param url_delete The image's deletion URL.
 * @param expires_at The image's expiration date.
 * @param uploaded_at The image's upload date.
 */
function save_image(name, storage_name, url, url_mini, url_delete, expires_at, uploaded_at)
{
    if (!window.localStorage) return;

    window.localStorage.setItem('izcraft.image.' + storage_name, JSON.stringify({
        'original_name': name,
        'url': url,
        'url_mini': url_mini,
        'url_delete': url_delete,
        'expires_at': expires_at,
        'uploaded_at': uploaded_at,
        'deleted': false
    }));
}

/**
 * Sets an image as deleted.
 *
 * @param storage_name The image's storage name.
 */
function set_image_deleted(storage_name)
{
    if (!window.localStorage) return;

    var data = window.localStorage.getItem('izcraft.image.' + storage_name);
    if (data)
    {
        data = JSON.parse(data);
        data['deleted'] = true;
        window.localStorage.setItem('izcraft.image.' + storage_name, JSON.stringify(data));
    }
}

/**
 * Returns all images uploaded from this browser.
 * @returns {Array}
 */
function get_images()
{
    if (!window.localStorage) return [];

    var images = [];

    for (var i = 0; i < window.localStorage.length; i++)
    {
        var key = window.localStorage.key(i);
        if (key.substr(0, 14) == 'izcraft.image.')
        {
            images.push(JSON.parse(window.localStorage.getItem(key)));
        }
    }

    images.sort(function (a, b) {
        var date_a = new Date(a['uploaded_at']);
        var date_b = new Date(b['uploaded_at']);

        return date_a.getTime() < date_b.getTime() ? -1 : 1;
    });

    return images;
}
