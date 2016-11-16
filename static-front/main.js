document.addEventListener("DOMContentLoaded", function() {
    setupClipboardActions();
    setupFileUpload();
    
    function setupFileUpload() {
        var fileLabel = document.getElementById("file");
        var fileInput = document.querySelector('#file input[type="file"]');
        var submitInput = document.querySelector('input[type="submit"]');
        var selectParagraph = document.getElementById("select_p");
        var infoParagraph = makeInfoParagraph();
        var validFileType = false;
        var validFileSize = false;
        
        if(!(fileLabel && fileInput && selectParagraph && submitInput)) return;
        
        document.body.className = "js";
        fileInput.addEventListener("change", updateForm);
        updateForm();
        
        function preventDrop(e) {
            if(e.target === fileInput) return;
            e.dataTransfer.dropEffect = 'none';
            e.preventDefault();
        }
        
        document.documentElement.addEventListener("dragenter", preventDrop);
        document.documentElement.addEventListener("dragover", preventDrop);
        
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
            
            var previewImage = infoParagraph.appendChild(document.createElement("img"));
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
                    
                    fileTypeInfo.innerHTML = "<strong>Type </strong><span>" + (selectedFile.type || "Inconnu");
                    fileSizeInfo.innerHTML = "<strong>Taille </strong><span>" + beautifySize(selectedFile.size);
                    
                    fileTypeInfo.className = validFileType ? "" : "invalid";
                    fileSizeInfo.className = validFileSize ? "" : "invalid";
                    submitInput.className = (validFileSize && validFileType) ? "" : "invalid";
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
    }
    
    function setupClipboardActions() {
        
    }
});
