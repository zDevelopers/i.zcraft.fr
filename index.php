<?php


echo '<?xml version="1.0" encoding="UTF-8" standalone="no"?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" lang="fr" xml:lang="fr">
  <head>
    <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
    <title>Hébergeur d\'images !</title>
  </head>';


// si message posté on affiche les liens
if(!empty($_FILES)) {
  if( !empty($_FILES['URL']['name']) ) {
	$extensions_valides = array( 'jpg' , 'jpeg' , 'gif' , 'png', 'bmp', 'ico');

	$extension_upload = strtolower(  substr(  strrchr($_FILES['URL']['name'], '.')  ,1)  );

	// si extension valide
	if ( in_array($extension_upload,$extensions_valides) )  {

		// image valide ?
		//print_r(getimagesize($_FILES['URL']['tmp_name']));
		if(@is_array(getimagesize($_FILES['URL']['tmp_name']))){
			$image = true;

			$nom = rand(10000,99999).time().".".$extension_upload;
			$resultat = move_uploaded_file($_FILES['URL']['tmp_name'],"static/".$nom);

			// redimensionnement
			if ($resultat) {
				system("/usr/bin/convert static/$nom -resize '300x300>' static/mini_$nom");
				echo "<h2>Lien direct</h2> <a href=\"https://i.zcraft.fr/$nom\">https://i.zcraft.fr/$nom</a>";
				echo "<h2>Lien direct miniature</h2> <a href=\"https://i.zcraft.fr/mini_$nom\">https://i.zcraft.fr/mini_$nom</a>";
				echo "<h2>Lien forum avec miniature</h2> <pre>[url=https://i.zcraft.fr/$nom][img]https://i.zcraft.fr/mini_$nom"."[/img][/url]</pre>";
                                echo "<h2>Lien forum avec miniature, centré</h2> <pre>[center][url=https://i.zcraft.fr/$nom][img]https://i.zcraft.fr/mini_$nom"."[/img][/url][/center]</pre>";
			} else { echo "échec"; }
		}
		else {
			echo "Le fichier n'est pas une image valide";
		}


	} else { echo "extension invalide."; }

  }
  else {
	echo "erreur !";
  }
}
else {
	// sinon on affiche le formulaire d'envoi
	echo ' <body>
		<h2 style="margin-left: 50px;">Téléversement d\'images</h2>
		<form action="/index.php" method="post" enctype="multipart/form-data" style="margin-left: 50px;">
		  Fichier <input type="file" name="URL" />
		  <input type="submit" value="Envoyer" />
		</form>';
}


echo '</body></html>';

