<?php

final class EditTaskDependenciesAPIMethod extends ManiphestConduitAPIMethod {

  public function getAPIMethodName() {
    return 'maniphest.editdependencies';
  }

  public function getMethodDescription() {
    return pht('Edit the dependencies for differential tasks.');
  }

  protected function defineParamTypes() {
    return [
      'taskPHID'            => 'required id',
      'dependsOnTaskPHIDs'  => 'optional list<phid>',
      'dependsOnCommits'    => 'optional list<string>',
      'dependsOnDiffs'      => 'optional list<string>',
    ];
  }

  protected function defineReturnType() {
    return 'nonempty dict';
  }

  protected function defineErrorTypes() {
    return [
      'ERR_GRAPH_CYCLE'   => pht(
        'The relationships between objects described in this request would creates a cycle in '.
        'their dependency graph.'),
      'ERR_BAD_REVISION'  => pht(
        'The specified revision PHID does not correspond to an existing differential revision.'),
    ];
  }

  protected function execute(ConduitAPIRequest $request) {
    $user = $request->getUser();
    $phid = $request->getValue('taskPHID');
    $attach_type = ManiphestTaskPHIDType::TYPECONST;

    $revision = (new ManiphestTaskQuery)
      ->setViewer($user)
      ->withPHIDs([$phid])
      ->executeOne();

    if (!$revision) {
      throw new ConduitException('ERR_BAD_REVISION');
    }

    $edge_type = ManiphestTaskDependsOnTaskEdgeType::EDGECONST;
    $cedge_type = ManiphestTaskHasCommitEdgeType::EDGECONST;
    $dedge_type = ManiphestTaskHasRevisionEdgeType::EDGECONST;

    //commits
    $cquery = id(new DiffusionCommitQuery())
      ->setViewer($user)
      ->needCommitData(true)
      ->withIdentifiers($request->getValue('dependsOnCommits'));

    $pager = $this->newPager($request);
    $commits = $cquery->executeWithCursorPager($pager);
    $commits = array_map(function($obj){ return $obj->getPHID();}, $commits);


    // Tasks
    $phids = $request->getValue('dependsOnTaskPHIDs', []);

    $old_phids = PhabricatorEdgeQuery::loadDestinationPHIDs(
      $phid,
      $edge_type);
    $add_phids = $phids;
    $rem_phids = array_diff($old_phids, $add_phids);
	
  
    // Diffs 
    $dquery = id(new DifferentialRevisionQuery())
		-> setViewer($user)
		-> withIDs($request->getValue('dependsOnDiffs'));


    $dquery->needRelationships(true);
    $dquery->needCommitPHIDs(true);
    $dquery->needDiffIDs(true);
    $dquery->needActiveDiffs(true);
    $dquery->needHashes(true);
    $revisions = $dquery->execute();
    $revisions = array_map(function($obj){ return $obj->getPHID();}, $revisions);



    $txn_editor = $revision->getApplicationTransactionEditor()
      ->setActor($user)
      ->setContentSource($request->newContentSource())
      ->setContinueOnMissingFields(true)
      ->setContinueOnNoEffect(true);
    // Tasks
    $txn_template = $revision->getApplicationTransactionTemplate()
      ->setTransactionType(PhabricatorTransactions::TYPE_EDGE)
      ->setMetadataValue('edge:type', $edge_type)
      ->setNewValue([
          '+' => array_fuse($add_phids),
          '-' => array_fuse($rem_phids),
        ]);

    try {
      $txn_editor->applyTransactions(
        $revision->getApplicationTransactionObject(),
        [$txn_template]);
    } catch (PhabricatorEdgeCycleException $ex) {
      throw new ConduitException('ERR_GRAPH_CYCLE');
    }

    // Commits
    $ctxn_template = $revision->getApplicationTransactionTemplate()
      ->setTransactionType(PhabricatorTransactions::TYPE_EDGE)
      ->setMetadataValue('edge:type', $cedge_type)
      ->setNewValue([
          '+' => array_fuse(array_values($commits)),
        ]);

    try {
      $txn_editor->applyTransactions(
        $revision->getApplicationTransactionObject(),
        [$ctxn_template]);
    } catch (PhabricatorEdgeCycleException $ex) {
      throw new ConduitException('ERR_GRAPH_CYCLE');
    }
    // Diffs

    $dtxn_template = $revision->getApplicationTransactionTemplate()
      ->setTransactionType(PhabricatorTransactions::TYPE_EDGE)
      ->setMetadataValue('edge:type', $dedge_type)
      ->setNewValue([
          '+' => array_fuse(array_values($revisions)),
        ]);

    try {
      $txn_editor->applyTransactions(
        $revision->getApplicationTransactionObject(),
        [$dtxn_template]);
    } catch (PhabricatorEdgeCycleException $ex) {
      throw new ConduitException('ERR_GRAPH_CYCLE');
    }

    return [
      'revision' => [
        'id' => $revision->getID(),
        'phid' => $revision->getPHID(),
        'dependencies' => $phids,
      ],
      'edits' => [
        'oldDependencies' => $old_phids,
        'newDependencies' => $add_phids,
        'removedDependencies' => $rem_phids,
        'cs' => $commits,
        'acs' => array_fuse($commits),
        'acss' => array_values($commits),
      ],
    ];
  }

}
