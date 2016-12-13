'use strict';

document.addEventListener("DOMContentLoaded", function() {
    (function setupFileUpload() {
        var fileLabel = document.getElementById("file");
        var fileInput = document.querySelector('#file input[type="file"]');
        var submitInput = document.querySelector('input[type="submit"]');
        var selectParagraph = document.getElementById("select_p");
        var deletionSelect = document.getElementById("expires_after");
        var deletionCheckbox = document.getElementById("expires");
        var infoParagraph = makeInfoParagraph();
        var validFileType = false;
        var validFileSize = false;
        var dragStatus = 0;
        
        if(!(fileLabel && fileInput && selectParagraph && submitInput && deletionSelect && deletionCheckbox)) return;
        
        document.body.className = "js";
        fileInput.addEventListener("change", updateForm);
        updateForm();
        
        function preventDrop(e) {
            if(e.target === fileInput) return;
            e.dataTransfer.dropEffect = 'none';
        }
        function dragStart(e) {
            ++dragStatus;
            document.body.classList.add('dragging');
        }

        function dragStop(e) {
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
        

        deletionSelect.addEventListener("change", function(e) {
            deletionCheckbox.checked = true;
        });

        function updateForm() {
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
        
        function makeInfoParagraph() {
            var infoParagraph = document.createElement("p");
            
            var previewContainer = infoParagraph.appendChild(document.createElement("figure"));
            var previewImage = previewContainer.appendChild(document.createElement("img"));
            var infoList = infoParagraph.appendChild(document.createElement("ul"));
            var fileNameInfo = infoList.appendChild(document.createElement("li"));
            var fileTypeInfo = infoList.appendChild(document.createElement("li"));
            var fileSizeInfo = infoList.appendChild(document.createElement("li"));
            var selectOtherParagraph = infoParagraph.appendChild(document.createElement("p"));
            selectOtherParagraph.innerHTML = '<em>Glissez-déposez</em> ou <em>cliquez</em> dans la zone pour sélectionner une autre image</p>';
            
            infoParagraph.id = "info_p";
            infoParagraph.updateInfo = function() {
                fileNameInfo.innerHTML = "";
                fileNameInfo.appendChild(document.createTextNode(fileInput.value));
                if(fileInput.files && fileInput.files[0]) {
                    var selectedFile = fileInput.files[0];
                    validFileSize = selectedFile.size <= 5242880;
                    validFileType = selectedFile.type.startsWith("image/");
                    
                    fileTypeInfo.innerHTML = "Fichier <code>" + (selectedFile.type || "Inconnu");
                    fileSizeInfo.innerHTML = beautifySize(selectedFile.size);
                    
                    fileTypeInfo.className = validFileType ? "" : "invalid";
                    fileSizeInfo.className = validFileSize ? "" : "invalid";
                    submitInput.className = (validFileSize && validFileType) ? "" : "invalid";
                    infoParagraph.className = (validFileSize && validFileType) ? "" : "invalid";
                    submitInput.disabled = !(validFileSize && validFileType);
                    
                    if(validFileType) {
                        var reader = new FileReader();
                        reader.onload = function(e) {previewImage.src = e.target.result;};
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
        
        function beautifySize(size) {
            var units = ['octets', 'Kio', 'Mio', 'Gio', 'Tio'];
            var currentUnit = 0;
            
            while (size > 1024) {
                ++currentUnit;
                size /= 1024;
            }
            
            return size.toFixed(1) + ' ' + units[currentUnit];
        }
    })();
    
    (function setupClipboardActions() {
        function selectText(element) {
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
            button.textContent = "C";
            button.title = "Copier dans le presse-papiers";
            
            button.addEventListener("click", function(button) {
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
                button.timeoutId = setTimeout(function() {
                    button.className = "";
                }, 2000);
                
                document.body.removeChild(copyBuffer);
            });


            linkblock.firstElementChild.addEventListener("click", function(e) {
                selectText(e.target);
            });
        }
    })();
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
 * @param uploaded_at The image's expiration date.
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

    images.sort(function(a, b)
    {
        var date_a = new Date(a['uploaded_at']);
        var date_b = new Date(b['uploaded_at']);

        return date_a.getTime() < date_b.getTime() ? -1 : 1;
    });

    return images;
}
